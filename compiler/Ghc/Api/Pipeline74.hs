{-# LANGUAGE BangPatterns, CPP, NamedFieldPuns #-}
{-# OPTIONS -fno-cse #-}
module Ghc.Api.Pipeline74 where

#include "HsVersions.h"

import Config ( cProjectVersionInt, cGccLinkerOpts,  )
import DriverPhases -- ( Phase(..), happensBefore, eqPhase, startPhase, phaseInputExt )
import DynFlags
import ErrUtils ( debugTraceMsg )
import FastString ( sLit )
import Finder ( mkHomeModLocation2, addHomeModuleToFinder )
import HeaderInfo
import HscMain
import HscTypes
import Module ( ModLocation(..), mkModuleName, addBootSuffixLocn )
import Outputable
import Packages ( getPackageIncludePath )
import Panic ( GhcException(..), panic, ghcError )
import ParserCoreUtils  ( getCoreModuleName )
import SrcLoc ( Located(..) )
import StringBuffer ( hGetStringBuffer )
import SysTools ( copyWithHeader, newTempName, figureLlvmVersion )
import Util ( Suffix, reslash, Direction(..) )
import qualified SysTools
import SrcLoc
import Platform

import Control.Exception
import Control.Monad
import Data.Maybe
import System.Directory
import System.FilePath

import Ghc.Api.Pipeline.Types

compileFiles :: HscEnv -> PhaseImplementations -> Phase -> [Source] -> IO ()
compileFiles hsc_env hooks stop_phase sources = do
  outputFileNames <- mapM (compileFile hsc_env hooks stop_phase) sources
  -- TODO: do link
  return ()

compileFile :: HscEnv -> PhaseImplementations
            -> Phase -> Source
            -> IO FilePath
compileFile hsc_env hooks stop_phase source = do
   let !src = sourceFileName source
       !mb_phase = sourceOptStopPhase source
   exists <- doesFileExist src
   when (not exists) $
        ghcError (CmdLineError ("does not exist: " ++ src))

   let
        dflags = hsc_dflags hsc_env
        split     = dopt Opt_SplitObjs dflags
        mb_o_file = outputFile dflags
        ghc_link  = ghcLink dflags      -- Set by -c or -no-link

        -- When linking, the -o argument refers to the linker's output.
        -- otherwise, we use it as the name for the pipeline's output.
        output
         | StopLn <- stop_phase, not (isNoLink ghc_link) = Persistent
                -- -o foo applies to linker
         | Just o_file <- mb_o_file = SpecificFile o_file
                -- -o foo applies to the file we are compiling now
         | otherwise = Persistent

        stop_phase' = case stop_phase of
                        As | split -> SplitAs
                        _          -> stop_phase

   ( _, out_file) <- runPipeline stop_phase' hsc_env hooks
                            (src, mb_phase) Nothing output
                            Nothing{-no ModLocation-} Nothing
   return out_file


data PipelineOutput
  = Temporary
        -- ^ Output should be to a temporary file: we're going to
        -- run more compilation steps on this output later.
  | Persistent
        -- ^ We want a persistent file, i.e. a file in the current directory
        -- derived from the input filename, but with the appropriate extension.
        -- eg. in "ghc -c Foo.hs" the output goes into ./Foo.o.
  | SpecificFile FilePath
        -- ^ The output must go into the specified file.


-- | Run a compilation pipeline, consisting of multiple phases.
--
-- This is the interface to the compilation pipeline, which runs
-- a series of compilation steps on a single source file, specifying
-- at which stage to stop.
--
-- The DynFlags can be modified by phases in the pipeline (eg. by
-- OPTIONS_GHC pragmas), and the changes affect later phases in the
-- pipeline.
runPipeline
  :: Phase                      -- ^ When to stop
  -> HscEnv                     -- ^ Compilation environment
  -> PhaseImplementations
  -> (FilePath,Maybe Phase)     -- ^ Input filename (and maybe -x suffix)
  -> Maybe FilePath             -- ^ original basename (if different from ^^^)
  -> PipelineOutput             -- ^ Output filename
  -> Maybe ModLocation          -- ^ A ModLocation, if this is a Haskell module
  -> Maybe FilePath             -- ^ stub object, if we have one
  -> IO (DynFlags, FilePath)     -- ^ (final flags, output filename)

runPipeline stop_phase hsc_env0 hooks (input_fn, mb_phase)
            mb_basename output maybe_loc maybe_stub_o
  = do
  let dflags0 = hsc_dflags hsc_env0
      (input_basename, suffix) = splitExtension input_fn
      suffix' = drop 1 suffix -- strip off the .
      basename | Just b <- mb_basename = b
               | otherwise             = input_basename

      -- Decide where dump files should go based on the pipeline output
      dflags = dflags0 { dumpPrefix = Just (basename ++ ".") }
      hsc_env = hsc_env0 {hsc_dflags = dflags}

        -- If we were given a -x flag, then use that phase to start from
      start_phase = fromMaybe (startPhase suffix') mb_phase

  -- We want to catch cases of "you can't get there from here" before
  -- we start the pipeline, because otherwise it will just run off the
  -- end.
  --
  -- There is a partial ordering on phases, where A < B iff A occurs
  -- before B in a normal compilation pipeline.

  when (not (start_phase `happensBefore` stop_phase)) $
        ghcError (UsageError
                    ("cannot compile this file to desired target: "
                       ++ input_fn))

  -- this is a function which will be used to calculate output file names
  -- as we go along (we partially apply it to some of its inputs here)
  let get_output_fn = getOutputFilename stop_phase output basename

  -- Execute the pipeline...
  let env   = PipeEnv{ stop_phase,
                       src_basename = basename,
                       src_suffix = suffix',
                       output_spec = output,
                       hooks = hooks }

      state = PipeState{ hsc_env, maybe_loc, maybe_stub_o = maybe_stub_o }

  (state', output_fn) <- unP (pipeLoop start_phase input_fn) env state

  let PipeState{ hsc_env=hsc_env', maybe_loc } = state'
      dflags' = hsc_dflags hsc_env'

  -- Sometimes, a compilation phase doesn't actually generate any output
  -- (eg. the CPP phase when -fcpp is not turned on).  If we end on this
  -- stage, but we wanted to keep the output, then we have to explicitly
  -- copy the file, remembering to prepend a {-# LINE #-} pragma so that
  -- further compilation stages can tell what the original filename was.
  case output of
    Temporary ->
        return (dflags', output_fn)
    _other -> 
        do final_fn <- get_output_fn dflags' stop_phase maybe_loc
           when (final_fn /= output_fn) $ do
              let msg = ("Copying `" ++ output_fn ++"' to `" ++ final_fn ++ "'")
                  line_prag = Just ("{-# LINE 1 \"" ++ input_fn ++ "\" #-}\n")
              copyWithHeader dflags msg line_prag output_fn final_fn
           return (dflags', final_fn)


-- In each phase, we need to know into what filename to generate the
-- output.  All the logic about which filenames we generate output
-- into is embodied in the following function.

getOutputFilename
  :: Phase -> PipelineOutput -> String
  -> DynFlags -> Phase{-next phase-} -> Maybe ModLocation -> IO FilePath
getOutputFilename stop_phase output basename
 = func
 where
        func dflags next_phase maybe_location
           | is_last_phase, Persistent <- output     = persistent_fn
           | is_last_phase, SpecificFile f <- output = return f
           | keep_this_output                        = persistent_fn
           | otherwise                               = newTempName dflags suffix
           where
                hcsuf      = hcSuf dflags
                odir       = objectDir dflags
                osuf       = objectSuf dflags
                keep_hc    = dopt Opt_KeepHcFiles dflags
                keep_s     = dopt Opt_KeepSFiles dflags
                keep_bc    = dopt Opt_KeepLlvmFiles dflags

                myPhaseInputExt HCc       = hcsuf
                myPhaseInputExt MergeStub = osuf
                myPhaseInputExt StopLn    = osuf
                myPhaseInputExt other     = phaseInputExt other

                is_last_phase = next_phase `eqPhase` stop_phase

                -- sometimes, we keep output from intermediate stages
                keep_this_output =
                     case next_phase of
                             As      | keep_s     -> True
                             LlvmOpt | keep_bc    -> True
                             HCc     | keep_hc    -> True
                             _other               -> False

                suffix = myPhaseInputExt next_phase

                -- persistent object files get put in odir
                persistent_fn
                   | StopLn <- next_phase = return odir_persistent
                   | otherwise            = return persistent

                persistent = basename <.> suffix

                odir_persistent
                   | Just loc <- maybe_location = ml_obj_file loc
                   | Just d <- odir = d </> persistent
                   | otherwise      = persistent

-- PipeEnv: invariant information passed down
data PipeEnv = PipeEnv {
       stop_phase   :: Phase,       -- ^ Stop just before this phase
       src_basename :: String,      -- ^ basename of original input source
       src_suffix   :: String,      -- ^ its extension
       output_spec  :: PipelineOutput, -- ^ says where to put the pipeline output
       hooks :: PhaseImplementations
  }

-- PipeState: information that might change during a pipeline run
data PipeState = PipeState {
       hsc_env   :: HscEnv,
          -- ^ only the DynFlags change in the HscEnv.  The DynFlags change
          -- at various points, for example when we read the OPTIONS_GHC
          -- pragmas in the Cpp phase.
       maybe_loc :: Maybe ModLocation,
          -- ^ the ModLocation.  This is discovered during compilation,
          -- in the Hsc phase where we read the module header.
       maybe_stub_o :: Maybe FilePath
          -- ^ the stub object.  This is set by the Hsc phase if a stub
          -- object was created.  The stub object will be joined with
          -- the main compilation object using "ld -r" at the end.
  }

getPipeEnv :: CompPipeline PipeEnv
getPipeEnv = P $ \env state -> return (state, env)

getPipeState :: CompPipeline PipeState
getPipeState = P $ \_env state -> return (state, state)

getDynFlags :: CompPipeline DynFlags
getDynFlags = P $ \_env state -> return (state, hsc_dflags (hsc_env state))

setDynFlags :: DynFlags -> CompPipeline ()
setDynFlags dflags = P $ \_env state ->
  return (state{hsc_env= (hsc_env state){ hsc_dflags = dflags }}, ())

setModLocation :: ModLocation -> CompPipeline ()
setModLocation loc = P $ \_env state ->
  return (state{ maybe_loc = Just loc }, ())

setStubO :: FilePath -> CompPipeline ()
setStubO stub_o = P $ \_env state ->
  return (state{ maybe_stub_o = Just stub_o }, ())

newtype CompPipeline a = P { unP :: PipeEnv -> PipeState -> IO (PipeState, a) }

instance Monad CompPipeline where
  return a = P $ \_env state -> return (state, a)
  P m >>= k = P $ \env state -> do (state',a) <- m env state
                                   unP (k a) env state'

io :: IO a -> CompPipeline a
io m = P $ \_env state -> do a <- m; return (state, a)

phaseOutputFilename :: Phase{-next phase-} -> CompPipeline FilePath
phaseOutputFilename next_phase = do
  PipeEnv{stop_phase, src_basename, output_spec} <- getPipeEnv
  PipeState{maybe_loc, hsc_env} <- getPipeState
  let dflags = hsc_dflags hsc_env
  io $ getOutputFilename stop_phase output_spec
                         src_basename dflags next_phase maybe_loc


-- | pipeLoop runs phases until we reach the stop phase
pipeLoop :: Phase -> FilePath -> CompPipeline FilePath
pipeLoop phase input_fn = do
  PipeEnv{stop_phase} <- getPipeEnv
  PipeState{hsc_env}  <- getPipeState
  case () of
   _ | phase `eqPhase` stop_phase            -- All done
     -> return input_fn

     | not (phase `happensBefore` stop_phase)
        -- Something has gone wrong.  We'll try to cover all the cases when
        -- this could happen, so if we reach here it is a panic.
        -- eg. it might happen if the -C flag is used on a source file that
        -- has {-# OPTIONS -fasm #-}.
     -> panic ("pipeLoop: at phase " ++ show phase ++
           " but I wanted to stop at phase " ++ show stop_phase)

     | otherwise
     -> do io $ debugTraceMsg (hsc_dflags hsc_env) 4
                         (ptext (sLit "Running phase") <+> ppr phase)
           dflags <- getDynFlags
           (next_phase, output_fn) <- runPhase phase input_fn dflags
           pipeLoop next_phase output_fn


-- | Each phase in the pipeline returns the next phase to execute, and the
-- name of the file in which the output was placed.
--
-- We must do things dynamically this way, because we often don't know
-- what the rest of the phases will be until part-way through the
-- compilation: for example, an {-# OPTIONS -fasm #-} at the beginning
-- of a source file can change the latter stages of the pipeline from
-- taking the via-C route to using the native code generator.
--
runPhase :: Phase       -- ^ Run this phase
         -> FilePath    -- ^ name of the input file
         -> DynFlags    -- ^ for convenience, we pass the current dflags in
         -> CompPipeline (Phase,               -- next phase to run
                          FilePath)            -- output filename

        -- Invariant: the output filename always contains the output
        -- Interesting case: Hsc when there is no recompilation to do
        --                   Then the output filename is still a .o file


-------------------------------------------------------------------------------
-- Unlit phase

runPhase (Unlit sf) input_fn dflags
  = do
       output_fn <- phaseOutputFilename (Cpp sf)

       let unlit_flags = getOpts dflags opt_L
           flags = map SysTools.Option unlit_flags ++
                   [ -- The -h option passes the file name for unlit to
                     -- put in a #line directive
                     SysTools.Option     "-h"
                   , SysTools.Option $ escape $ normalise input_fn
                   , SysTools.FileOption "" input_fn
                   , SysTools.FileOption "" output_fn
                   ]

       io $ SysTools.runUnlit dflags flags

       return (Cpp sf, output_fn)
  where
       -- escape the characters \, ", and ', but don't try to escape
       -- Unicode or anything else (so we don't use Util.charToC
       -- here).  If we get this wrong, then in
       -- Coverage.addTicksToBinds where we check that the filename in
       -- a SrcLoc is the same as the source filenaame, the two will
       -- look bogusly different. See test:
       -- libraries/hpc/tests/function/subdir/tough2.lhs
       escape ('\\':cs) = '\\':'\\': escape cs
       escape ('\"':cs) = '\\':'\"': escape cs
       escape ('\'':cs) = '\\':'\'': escape cs
       escape (c:cs)    = c : escape cs
       escape []        = []

-------------------------------------------------------------------------------
-- Cpp phase : (a) gets OPTIONS out of file
--             (b) runs cpp if necessary

runPhase (Cpp sf) input_fn dflags0
  = do
       src_opts <- io $ getOptionsFromFile dflags0 input_fn
       (dflags1, unhandled_flags, warns)
           <- io $ parseDynamicFilePragma dflags0 src_opts
       setDynFlags dflags1
       io $ checkProcessArgsResult unhandled_flags

       if not (xopt Opt_Cpp dflags1) then do
           -- we have to be careful to emit warnings only once.
           unless (dopt Opt_Pp dflags1) $ io $ handleFlagWarnings dflags1 warns

           -- no need to preprocess CPP, just pass input file along
           -- to the next phase of the pipeline.
           return (HsPp sf, input_fn)
        else do
            output_fn <- phaseOutputFilename (HsPp sf)
            io $ doCpp dflags1 True{-raw-} False{-no CC opts-} input_fn output_fn
            -- re-read the pragmas now that we've preprocessed the file
            -- See #2464,#3457
            src_opts <- io $ getOptionsFromFile dflags0 output_fn
            (dflags2, unhandled_flags, warns)
                <- io $ parseDynamicFilePragma dflags0 src_opts
            io $ checkProcessArgsResult unhandled_flags
            unless (dopt Opt_Pp dflags2) $ io $ handleFlagWarnings dflags2 warns
            -- the HsPp pass below will emit warnings

            setDynFlags dflags2

            return (HsPp sf, output_fn)

-------------------------------------------------------------------------------
-- HsPp phase

runPhase (HsPp sf) input_fn dflags
  = do
       if not (dopt Opt_Pp dflags) then
           -- no need to preprocess, just pass input file along
           -- to the next phase of the pipeline.
          return (Hsc sf, input_fn)
        else do
            let hspp_opts = getOpts dflags opt_F
            PipeEnv{src_basename, src_suffix} <- getPipeEnv
            let orig_fn = src_basename <.> src_suffix
            output_fn <- phaseOutputFilename (Hsc sf)
            io $ SysTools.runPp dflags
                           ( [ SysTools.Option     orig_fn
                             , SysTools.Option     input_fn
                             , SysTools.FileOption "" output_fn
                             ] ++
                             map SysTools.Option hspp_opts
                           )

            -- re-read pragmas now that we've parsed the file (see #3674)
            src_opts <- io $ getOptionsFromFile dflags output_fn
            (dflags1, unhandled_flags, warns)
                <- io $ parseDynamicFilePragma dflags src_opts
            setDynFlags dflags1
            io $ checkProcessArgsResult unhandled_flags
            io $ handleFlagWarnings dflags1 warns

            return (Hsc sf, output_fn)

-----------------------------------------------------------------------------
-- Hsc phase

-- Compilation of a single module, in "legacy" mode (_not_ under
-- the direction of the compilation manager).
runPhase (Hsc src_flavour) input_fn dflags0
 = do   -- normal Hsc mode, not mkdependHS

        PipeEnv{ stop_phase=stop,
                 src_basename=basename,
                 src_suffix=suff } <- getPipeEnv

  -- we add the current directory (i.e. the directory in which
  -- the .hs files resides) to the include path, since this is
  -- what gcc does, and it's probably what you want.
        let current_dir = case takeDirectory basename of
                     "" -> "." -- XXX Hack required for filepath-1.1 and earlier
                               -- (GHC 6.12 and earlier)
                     d -> d

            paths = includePaths dflags0
            dflags = dflags0 { includePaths = current_dir : paths }

        setDynFlags dflags

  -- gather the imports and module name
        (hspp_buf,mod_name,imps,src_imps) <- io $
            case src_flavour of
                ExtCoreFile -> do  -- no explicit imports in ExtCore input.
                    m <- getCoreModuleName input_fn
                    return (Nothing, mkModuleName m, [], [])

                _           -> do
                    buf <- hGetStringBuffer input_fn
                    (src_imps,imps,L _ mod_name) <- getImports dflags buf input_fn (basename <.> suff)
                    return (Just buf, mod_name, imps, src_imps)

  -- Build a ModLocation to pass to hscMain.
  -- The source filename is rather irrelevant by now, but it's used
  -- by hscMain for messages.  hscMain also needs
  -- the .hi and .o filenames, and this is as good a way
  -- as any to generate them, and better than most. (e.g. takes
  -- into accout the -osuf flags)
        location1 <- io $ mkHomeModLocation2 dflags mod_name basename suff

  -- Boot-ify it if necessary
        let location2 | isHsBoot src_flavour = addBootSuffixLocn location1
                      | otherwise            = location1


  -- Take -ohi into account if present
  -- This can't be done in mkHomeModuleLocation because
  -- it only applies to the module being compiles
        let ohi = outputHi dflags
            location3 | Just fn <- ohi = location2{ ml_hi_file = fn }
                      | otherwise      = location2

  -- Take -o into account if present
  -- Very like -ohi, but we must *only* do this if we aren't linking
  -- (If we're linking then the -o applies to the linked thing, not to
  -- the object file for one module.)
  -- Note the nasty duplication with the same computation in compileFile above
        let expl_o_file = outputFile dflags
            location4 | Just ofile <- expl_o_file
                      , isNoLink (ghcLink dflags)
                      = location3 { ml_obj_file = ofile }
                      | otherwise = location3

            o_file = ml_obj_file location4      -- The real object file

        setModLocation location4

  -- Figure out if the source has changed, for recompilation avoidance.
  --
  -- Setting source_unchanged to True means that M.o seems
  -- to be up to date wrt M.hs; so no need to recompile unless imports have
  -- changed (which the compiler itself figures out).
  -- Setting source_unchanged to False tells the compiler that M.o is out of
  -- date wrt M.hs (or M.o doesn't exist) so we must recompile regardless.
        src_timestamp <- io $ getModificationTime (basename <.> suff)

        let hsc_lang = hscTarget dflags
        source_unchanged <- io $
          if not (isStopLn stop)
                -- SourceModified unconditionally if
                --      (a) recompilation checker is off, or
                --      (b) we aren't going all the way to .o file (e.g. ghc -S)
             then return SourceModified
                -- Otherwise look at file modification dates
             else do o_file_exists <- doesFileExist o_file
                     if not o_file_exists
                        then return SourceModified       -- Need to recompile
                        else do t2 <- getModificationTime o_file
                                if t2 > src_timestamp
                                  then return SourceUnmodified
                                  else return SourceModified

  -- get the DynFlags
        let next_phase = hscPostBackendPhase dflags src_flavour hsc_lang
        output_fn  <- phaseOutputFilename next_phase

        let dflags' = dflags { hscTarget = hsc_lang,
                               hscOutName = output_fn,
                               extCoreName = basename ++ ".hcr" }

        setDynFlags dflags'
        PipeState{hsc_env=hsc_env'} <- getPipeState

  -- Tell the finder cache about this module
        mod <- io $ addHomeModuleToFinder hsc_env' mod_name location4

  -- Make the ModSummary to hand to hscMain
        let
            mod_summary = ModSummary {  ms_mod       = mod,
                                        ms_hsc_src   = src_flavour,
                                        ms_hspp_file = input_fn,
                                        ms_hspp_opts = dflags,
                                        ms_hspp_buf  = hspp_buf,
                                        ms_location  = location4,
                                        ms_hs_date   = src_timestamp,
                                        ms_obj_date  = Nothing,
                                        ms_textual_imps = imps,
                                        ms_srcimps      = src_imps }

  -- run the compiler!
        result <- io $ hscCompileOneShot hsc_env'
                          mod_summary source_unchanged
                          Nothing       -- No iface
                          Nothing       -- No "module i of n" progress info

        case result of
          HscNoRecomp
              -> do io $ touchObjectFile dflags' o_file
                    -- The .o file must have a later modification date
                    -- than the source file (else we wouldn't be in HscNoRecomp)
                    -- but we touch it anyway, to keep 'make' happy (we think).
                    return (StopLn, o_file)
          (HscRecomp hasStub _)
              -> do case hasStub of
                      Nothing -> return ()
                      Just stub_c ->
                         do env <- getPipeEnv
                            stub_o <- io $ compileStub hsc_env' (hooks env) stub_c
                            setStubO stub_o
                    -- In the case of hs-boot files, generate a dummy .o-boot
                    -- stamp file for the benefit of Make
                    when (isHsBoot src_flavour) $
                      io $ touchObjectFile dflags' o_file
                    return (next_phase, output_fn)

-----------------------------------------------------------------------------
-- Cmm phase
{-
runPhase CmmCpp input_fn dflags
  = do
       output_fn <- phaseOutputFilename Cmm
       io $ doCpp dflags False{-not raw-} True{-include CC opts-}
              input_fn output_fn
       return (Cmm, output_fn)

runPhase Cmm input_fn dflags
  = do
        PipeEnv{src_basename} <- getPipeEnv
        let hsc_lang = hscTarget dflags

        let next_phase = hscPostBackendPhase dflags HsSrcFile hsc_lang

        output_fn <- phaseOutputFilename next_phase

        let dflags' = dflags { hscTarget = hsc_lang,
                               hscOutName = output_fn,
                               extCoreName = src_basename ++ ".hcr" }

        setDynFlags dflags'
        PipeState{hsc_env} <- getPipeState

        io $ hscCompileCmmFile hsc_env input_fn

        return (next_phase, output_fn)
-}
-----------------------------------------------------------------------------
-- Cc phase
{-
-- we don't support preprocessing .c files (with -E) now.  Doing so introduces
-- way too many hacks, and I can't say I've ever used it anyway.

runPhase cc_phase input_fn dflags
   | any (cc_phase `eqPhase`) [Cc, Ccpp, HCc, Cobjc, Cobjcpp]
   = do
        let platform = targetPlatform dflags
            cc_opts = getOpts dflags opt_c
            hcc = cc_phase `eqPhase` HCc

        let cmdline_include_paths = includePaths dflags

        -- HC files have the dependent packages stamped into them
        pkgs <- if hcc then io $ getHCFilePackages input_fn else return []

        -- add package include paths even if we're just compiling .c
        -- files; this is the Value Add(TM) that using ghc instead of
        -- gcc gives you :)
        pkg_include_dirs <- io $ getPackageIncludePath dflags pkgs
        let include_paths = foldr (\ x xs -> "-I" : x : xs) []
                              (cmdline_include_paths ++ pkg_include_dirs)

        let gcc_extra_viac_flags = extraGccViaCFlags dflags
        let pic_c_flags = picCCOpts dflags

        let verbFlags = getVerbFlags dflags

        -- cc-options are not passed when compiling .hc files.  Our
        -- hc code doesn't not #include any header files anyway, so these
        -- options aren't necessary.
        pkg_extra_cc_opts <- io $
          if cc_phase `eqPhase` HCc
             then return []
             else getPackageExtraCcOpts dflags pkgs

        framework_paths <-
            case platformOS platform of
            OSDarwin ->
                do pkgFrameworkPaths <- io $ getPackageFrameworkPath dflags pkgs
                   let cmdlineFrameworkPaths = frameworkPaths dflags
                   return $ map ("-F"++)
                                (cmdlineFrameworkPaths ++ pkgFrameworkPaths)
            _ ->
                return []

        let split_objs = dopt Opt_SplitObjs dflags
            split_opt | hcc && split_objs = [ "-DUSE_SPLIT_MARKERS" ]
                      | otherwise         = [ ]

        let cc_opt | optLevel dflags >= 2 = "-O2"
                   | otherwise            = "-O"

        -- Decide next phase
        let next_phase = As
        output_fn <- phaseOutputFilename next_phase

        let
          more_hcc_opts =
                -- on x86 the floating point regs have greater precision
                -- than a double, which leads to unpredictable results.
                -- By default, we turn this off with -ffloat-store unless
                -- the user specified -fexcess-precision.
                (if platformArch platform == ArchX86 &&
                    not (dopt Opt_ExcessPrecision dflags)
                        then [ "-ffloat-store" ]
                        else []) ++

                -- gcc's -fstrict-aliasing allows two accesses to memory
                -- to be considered non-aliasing if they have different types.
                -- This interacts badly with the C code we generate, which is
                -- very weakly typed, being derived from C--.
                ["-fno-strict-aliasing"]

        let gcc_lang_opt | cc_phase `eqPhase` Ccpp  = "c++"
                         | cc_phase `eqPhase` Cobjc = "objective-c"
                         | cc_phase `eqPhase` Cobjcpp = "objective-c++"
                         | otherwise                = "c"
        io $ SysTools.runCc dflags (
                -- force the C compiler to interpret this file as C when
                -- compiling .hc files, by adding the -x c option.
                -- Also useful for plain .c files, just in case GHC saw a
                -- -x c option.
                        [ SysTools.Option "-x", SysTools.Option gcc_lang_opt
                        , SysTools.FileOption "" input_fn
                        , SysTools.Option "-o"
                        , SysTools.FileOption "" output_fn
                        ]
                       ++ map SysTools.Option (
                          pic_c_flags

                -- Stub files generated for foreign exports references the runIO_closure
                -- and runNonIO_closure symbols, which are defined in the base package.
                -- These symbols are imported into the stub.c file via RtsAPI.h, and the
                -- way we do the import depends on whether we're currently compiling
                -- the base package or not.
                       ++ (if platformOS platform == OSMinGW32 &&
                              thisPackage dflags == basePackageId
                                then [ "-DCOMPILING_BASE_PACKAGE" ]
                                else [])

        -- We only support SparcV9 and better because V8 lacks an atomic CAS
        -- instruction. Note that the user can still override this
        -- (e.g., -mcpu=ultrasparc) as GCC picks the "best" -mcpu flag
        -- regardless of the ordering.
        --
        -- This is a temporary hack.
                       ++ (if platformArch platform == ArchSPARC
                           then ["-mcpu=v9"]
                           else [])

                       ++ (if hcc
                             then gcc_extra_viac_flags ++ more_hcc_opts
                             else [])
                       ++ verbFlags
                       ++ [ "-S", "-Wimplicit", cc_opt ]
                       ++ [ "-D__GLASGOW_HASKELL__="++cProjectVersionInt ]
                       ++ framework_paths
                       ++ cc_opts
                       ++ split_opt
                       ++ include_paths
                       ++ pkg_extra_cc_opts
                       ))

        return (next_phase, output_fn)
-}
-----------------------------------------------------------------------------
-- Splitting phase
{-
runPhase Splitter input_fn dflags
  = do  -- tmp_pfx is the prefix used for the split .s files

        split_s_prefix <- io $ SysTools.newTempName dflags "split"
        let n_files_fn = split_s_prefix

        io $ SysTools.runSplit dflags
                          [ SysTools.FileOption "" input_fn
                          , SysTools.FileOption "" split_s_prefix
                          , SysTools.FileOption "" n_files_fn
                          ]

        -- Save the number of split files for future references
        s <- io $ readFile n_files_fn
        let n_files = read s :: Int
            dflags' = dflags { splitInfo = Just (split_s_prefix, n_files) }

        setDynFlags dflags'

        -- Remember to delete all these files
        io $ addFilesToClean dflags' [ split_s_prefix ++ "__" ++ show n ++ ".s"
                                     | n <- [1..n_files]]

        return (SplitAs,
                "**splitter**") -- we don't use the filename in SplitAs
-}
-----------------------------------------------------------------------------
-- As, SpitAs phase : Assembler

-- This is for calling the assembler on a regular assembly file (not split).
runPhase As input_fn dflags
  = do
        -- LLVM from version 3.0 onwards doesn't support the OS X system
        -- assembler, so we use clang as the assembler instead. (#5636)
        let whichAsProg | hscTarget dflags == HscLlvm &&
                          platformOS (targetPlatform dflags) == OSDarwin
                        = do
                            llvmVer <- io $ figureLlvmVersion dflags
                            return $ case llvmVer of
                                -- using cGccLinkerOpts here but not clear if
                                -- opt_c isn't a better choice
                                Just n | n >= 30 ->
                                    (SysTools.runClang, cGccLinkerOpts)

                                _ -> (SysTools.runAs, getOpts dflags opt_a)

                        | otherwise
                        = return (SysTools.runAs, getOpts dflags opt_a)

        (as_prog, as_opts) <- whichAsProg
        let cmdline_include_paths = includePaths dflags

        next_phase <- maybeMergeStub
        output_fn <- phaseOutputFilename next_phase

        -- we create directories for the object file, because it
        -- might be a hierarchical module.
        io $ createDirectoryIfMissing True (takeDirectory output_fn)

        io $ as_prog dflags
                       (map SysTools.Option as_opts
                       ++ [ SysTools.Option ("-I" ++ p) | p <- cmdline_include_paths ]

        -- We only support SparcV9 and better because V8 lacks an atomic CAS
        -- instruction so we have to make sure that the assembler accepts the
        -- instruction set. Note that the user can still override this
        -- (e.g., -mcpu=ultrasparc). GCC picks the "best" -mcpu flag
        -- regardless of the ordering.
        --
        -- This is a temporary hack.
                       ++ (if platformArch (targetPlatform dflags) == ArchSPARC
                           then [SysTools.Option "-mcpu=v9"]
                           else [])

                       ++ [ SysTools.Option "-c"
                          , SysTools.FileOption "" input_fn
                          , SysTools.Option "-o"
                          , SysTools.FileOption "" output_fn
                          ])

        return (next_phase, output_fn)

{-
-- This is for calling the assembler on a split assembly file (so a collection
-- of assembly files)
runPhase SplitAs _input_fn dflags
  = do
        -- we'll handle the stub_o file in this phase, so don't MergeStub,
        -- just jump straight to StopLn afterwards.
        let next_phase = StopLn
        output_fn <- phaseOutputFilename next_phase

        let base_o = dropExtension output_fn
            osuf = objectSuf dflags
            split_odir  = base_o ++ "_" ++ osuf ++ "_split"

        io $ createDirectoryIfMissing True split_odir

        -- remove M_split/ *.o, because we're going to archive M_split/ *.o
        -- later and we don't want to pick up any old objects.
        fs <- io $ getDirectoryContents split_odir
        io $ mapM_ removeFile $
                map (split_odir </>) $ filter (osuf `isSuffixOf`) fs

        let as_opts = getOpts dflags opt_a

        let (split_s_prefix, n) = case splitInfo dflags of
                                  Nothing -> panic "No split info"
                                  Just x -> x

        let split_s   n = split_s_prefix ++ "__" ++ show n <.> "s"

            split_obj :: Int -> FilePath
            split_obj n = split_odir </>
                          takeFileName base_o ++ "__" ++ show n <.> osuf

        let assemble_file n
              = SysTools.runAs dflags
                         (map SysTools.Option as_opts ++

        -- We only support SparcV9 and better because V8 lacks an atomic CAS
        -- instruction so we have to make sure that the assembler accepts the
        -- instruction set. Note that the user can still override this
        -- (e.g., -mcpu=ultrasparc). GCC picks the "best" -mcpu flag
        -- regardless of the ordering.
        --
        -- This is a temporary hack.
                          (if platformArch (targetPlatform dflags) == ArchSPARC
                           then [SysTools.Option "-mcpu=v9"]
                           else []) ++

                          [ SysTools.Option "-c"
                          , SysTools.Option "-o"
                          , SysTools.FileOption "" (split_obj n)
                          , SysTools.FileOption "" (split_s n)
                          ])

        io $ mapM_ assemble_file [1..n]

        -- Note [pipeline-split-init]
        -- If we have a stub file, it may contain constructor
        -- functions for initialisation of this module.  We can't
        -- simply leave the stub as a separate object file, because it
        -- will never be linked in: nothing refers to it.  We need to
        -- ensure that if we ever refer to the data in this module
        -- that needs initialisation, then we also pull in the
        -- initialisation routine.
        --
        -- To that end, we make a DANGEROUS ASSUMPTION here: the data
        -- that needs to be initialised is all in the FIRST split
        -- object.  See Note [codegen-split-init].

        PipeState{maybe_stub_o} <- getPipeState
        case maybe_stub_o of
            Nothing     -> return ()
            Just stub_o -> io $ do
                     tmp_split_1 <- newTempName dflags osuf
                     let split_1 = split_obj 1
                     copyFile split_1 tmp_split_1
                     removeFile split_1
                     joinObjectFiles dflags [tmp_split_1, stub_o] split_1

        -- join them into a single .o file
        io $ joinObjectFiles dflags (map split_obj [1..n]) output_fn

        return (next_phase, output_fn)
-}
-----------------------------------------------------------------------------
-- LlvmOpt phase

-- runPhase LlvmOpt input_fn dflags
--   = do
--     let lo_opts = getOpts dflags opt_lo
--     let opt_lvl = max 0 (min 2 $ optLevel dflags)
--     -- don't specify anything if user has specified commands. We do this for
--     -- opt but not llc since opt is very specifically for optimisation passes
--     -- only, so if the user is passing us extra options we assume they know
--     -- what they are doing and don't get in the way.
--     let optFlag = if null lo_opts
--                      then [SysTools.Option (llvmOpts !! opt_lvl)]
--                      else []

--     output_fn <- phaseOutputFilename LlvmLlc

--     io $ SysTools.runLlvmOpt dflags
--                ([ SysTools.FileOption "" input_fn,
--                     SysTools.Option "-o",
--                     SysTools.FileOption "" output_fn]
--                 ++ optFlag
--                 ++ map SysTools.Option lo_opts)

--     return (LlvmLlc, output_fn)
--   where 
--         -- we always (unless -optlo specified) run Opt since we rely on it to
--         -- fix up some pretty big deficiencies in the code we generate
--         llvmOpts = ["-mem2reg", "-O1", "-O2"]

-----------------------------------------------------------------------------
-- LlvmLlc phase

-- runPhase LlvmLlc input_fn dflags
--   = do
--     let lc_opts = getOpts dflags opt_lc
--         opt_lvl = max 0 (min 2 $ optLevel dflags)
--         rmodel | opt_PIC        = "pic"
--                | not opt_Static = "dynamic-no-pic"
--                | otherwise      = "static"

--     -- hidden debugging flag '-dno-llvm-mangler' to skip mangling
--     let next_phase = case dopt Opt_NoLlvmMangler dflags of
--                          False                            -> LlvmMangle
--                          True | dopt Opt_SplitObjs dflags -> Splitter
--                          True                             -> As
                        
--     output_fn <- phaseOutputFilename next_phase

--     io $ SysTools.runLlvmLlc dflags
--                 ([ SysTools.Option (llvmOpts !! opt_lvl),
--                     SysTools.Option $ "-relocation-model=" ++ rmodel,
--                     SysTools.FileOption "" input_fn,
--                     SysTools.Option "-o", SysTools.FileOption "" output_fn]
--                 ++ map SysTools.Option lc_opts
--                 ++ map SysTools.Option fpOpts)

--     return (next_phase, output_fn)
--   where
--         -- Bug in LLVM at O3 on OSX.
--         llvmOpts = if platformOS (targetPlatform dflags) == OSDarwin
--                    then ["-O1", "-O2", "-O2"]
--                    else ["-O1", "-O2", "-O3"]
--         -- On ARMv7 using LLVM, LLVM fails to allocate floating point registers
--         -- while compiling GHC source code. It's probably due to fact that it
--         -- does not enable VFP by default. Let's do this manually here
--         fpOpts = case platformArch (targetPlatform dflags) of 
--                    ArchARM ARMv7 ext -> if (elem VFPv3 ext)
--                                       then ["-mattr=+v7,+vfp3"]
--                                       else if (elem VFPv3D16 ext)
--                                            then ["-mattr=+v7,+vfp3,+d16"]
--                                            else []
--                    _               -> []

-----------------------------------------------------------------------------
-- LlvmMangle phase

-- runPhase LlvmMangle input_fn dflags
--   = do
--       let next_phase = if dopt Opt_SplitObjs dflags then Splitter else As
--       output_fn <- phaseOutputFilename next_phase
--       io $ llvmFixupAsm dflags input_fn output_fn
--       return (next_phase, output_fn)

-----------------------------------------------------------------------------
-- merge in stub objects

-- runPhase MergeStub input_fn dflags
--  = do
--      PipeState{maybe_stub_o} <- getPipeState
--      output_fn <- phaseOutputFilename StopLn
--      case maybe_stub_o of
--        Nothing ->
--          panic "runPhase(MergeStub): no stub"
--        Just stub_o -> do
--          io $ joinObjectFiles dflags [input_fn, stub_o] output_fn
--          return (StopLn, output_fn)

-- warning suppression
runPhase other _input_fn _dflags =
   panic ("runPhase: don't know how to run phase " ++ show other)

-- | What phase to run after one of the backend code generators has run
hscPostBackendPhase :: DynFlags -> HscSource -> HscTarget -> Phase
hscPostBackendPhase _ HsBootFile _    =  StopLn
hscPostBackendPhase dflags _ hsc_lang =
  case hsc_lang of
        HscC -> HCc
        HscAsm | dopt Opt_SplitObjs dflags -> Splitter
               | otherwise                 -> As
        HscLlvm        -> LlvmOpt
        HscNothing     -> StopLn
        HscInterpreted -> StopLn

-- Running CPP

doCpp :: DynFlags -> Bool -> Bool -> FilePath -> FilePath -> IO ()
doCpp dflags raw include_cc_opts input_fn output_fn = do
    let hscpp_opts = getOpts dflags opt_P
    let cmdline_include_paths = includePaths dflags

    pkg_include_dirs <- getPackageIncludePath dflags []
    let include_paths = foldr (\ x xs -> "-I" : x : xs) []
                          (cmdline_include_paths ++ pkg_include_dirs)

    let verbFlags = getVerbFlags dflags

    let cc_opts
          | include_cc_opts = getOpts dflags opt_c
          | otherwise       = []

    let cpp_prog args | raw       = SysTools.runCpp dflags args
                      | otherwise = SysTools.runCc dflags (SysTools.Option "-E" : args)

    let target_defs =
          [ "-D" ++ HOST_OS     ++ "_BUILD_OS=1",
            "-D" ++ HOST_ARCH   ++ "_BUILD_ARCH=1",
            "-D" ++ TARGET_OS   ++ "_HOST_OS=1",
            "-D" ++ TARGET_ARCH ++ "_HOST_ARCH=1" ]
        -- remember, in code we *compile*, the HOST is the same our TARGET,
        -- and BUILD is the same as our HOST.

    cpp_prog       (   map SysTools.Option verbFlags
                    ++ map SysTools.Option include_paths
                    ++ map SysTools.Option hsSourceCppOpts
                    ++ map SysTools.Option target_defs
                    ++ map SysTools.Option hscpp_opts
                    ++ map SysTools.Option cc_opts
                    ++ [ SysTools.Option     "-x"
                       , SysTools.Option     "c"
                       , SysTools.Option     input_fn
        -- We hackily use Option instead of FileOption here, so that the file
        -- name is not back-slashed on Windows.  cpp is capable of
        -- dealing with / in filenames, so it works fine.  Furthermore
        -- if we put in backslashes, cpp outputs #line directives
        -- with *double* backslashes.   And that in turn means that
        -- our error messages get double backslashes in them.
        -- In due course we should arrange that the lexer deals
        -- with these \\ escapes properly.
                       , SysTools.Option     "-o"
                       , SysTools.FileOption "" output_fn
                       ])

hsSourceCppOpts :: [String]
-- Default CPP defines in Haskell source
hsSourceCppOpts =
        [ "-D__GLASGOW_HASKELL__="++cProjectVersionInt ]


touchObjectFile :: DynFlags -> FilePath -> IO ()
touchObjectFile dflags path = do
  createDirectoryIfMissing True $ takeDirectory path
  SysTools.touch dflags "Touching object file" path

-- stub .h and .c files (for foreign export support)

-- The _stub.c file is derived from the haskell source file, possibly taking
-- into account the -stubdir option.
--
-- The object file created by compiling the _stub.c file is put into a
-- temporary file, which will be later combined with the main .o file
-- (see the MergeStubs phase).

compileStub :: HscEnv -> PhaseImplementations -> FilePath -> IO FilePath
compileStub hsc_env hooks stub_c = do
  (_, stub_o) <- runPipeline StopLn hsc_env hooks (stub_c,Nothing)  Nothing
                             Temporary Nothing{-no ModLocation-} Nothing
  return stub_o


maybeMergeStub :: CompPipeline Phase
maybeMergeStub
 = do
     PipeState{maybe_stub_o} <- getPipeState
     if isJust maybe_stub_o then return MergeStub else return StopLn

defaultPhaseImplementations :: PhaseImplementations
defaultPhaseImplementations = PhaseImplementations
  { runUnlit = error "NYI: runUnlit default hook"
  , runHsCompiler = defaultRunHsCompiler }

defaultRunHsCompiler :: HscEnv -> ModSummary -> SourceUnchanged
                     -> IO HscStatus
defaultRunHsCompiler hsc_env mod_summary source_unchanged = do
  hscCompileOneShot hsc_env mod_summary src_modified
                    Nothing       -- No iface
                    Nothing       -- No "module i of n" progress info
 where
   src_modified
     | source_unchanged = SourceUnmodified
     | otherwise        = SourceModified
