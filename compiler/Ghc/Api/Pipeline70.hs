{-# LANGUAGE BangPatterns, CPP #-}
{-# OPTIONS -fno-cse #-}
module Ghc.Api.Pipeline70 where
  ( Source(..),
    compileFiles,
    compileFile,

    PhaseImplementations(..),
    InputFilePath, OutputFilePath, SourceUnchanged,
    defaultPhaseImplementations,

    Phase(..),
    HscStatus, HscStatus'(..),
  )
where

#include "HsVersions.h"

import Ghc.Api.Pipeline.Types

import Config ( cProjectVersionInt )
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
import Panic ( GhcException(..), panic )
import ParserCoreUtils  ( getCoreModuleName )
import SrcLoc ( Located(..) )
import StringBuffer ( hGetStringBuffer )
import SysTools ( copyWithHeader, newTempName )
import Util ( Suffix, reslash, Direction(..) )
import qualified SysTools

import Control.Exception
import Control.Monad
import Data.Maybe
import System.Directory
import System.FilePath

data Source = Source
  { sourceFileName :: !FilePath
  , sourceOptStopPhase :: Maybe Phase
  }

compileFiles :: GhcMonad m =>
                HscEnv -> PhaseImplementations m -> Phase -> [Source] -> m ()
compileFiles hsc_env hooks stop_phase sources = do
  outputFileNames <- mapM (compileFile hsc_env hooks stop_phase) sources
  -- TODO: do link
  return ()

compileFile :: GhcMonad m =>
               HscEnv -> PhaseImplementations m -> Phase -> Source -> m FilePath
compileFile hsc_env hooks stop_phase source = do
   let !sourceFile = sourceFileName source
       !fileStopPhase = sourceOptStopPhase source
   liftIO (ensureFileExists sourceFile)

   let !dflags = hsc_dflags hsc_env
       !split     = dopt Opt_SplitObjs dflags
       !mb_o_file = outputFile dflags
       !ghc_link  = ghcLink dflags      -- Set by -c or -no-link

         -- When linking, the -o argument refers to the linker's output.
         -- otherwise, we use it as the name for the pipeline's output.
       !output
         | StopLn <- stop_phase, not (isNoLink ghc_link) = Persistent
                -- -o foo applies to linker
         | Just o_file <- mb_o_file = SpecificFile o_file
                -- -o foo applies to the file we are compiling now
         | otherwise = Persistent

       !stop_phase' = case stop_phase of
                        As | split -> SplitAs
                        _          -> stop_phase

   ( _, out_file) <- runPipeline hooks stop_phase' hsc_env
                            sourceFile fileStopPhase
                            Nothing output
                            Nothing{-no ModLocation-}
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
runPipeline :: GhcMonad m =>
     PhaseImplementations m
  -> Phase                      -- ^ When to stop
  -> HscEnv                     -- ^ Compilation environment
  -> FilePath                   -- ^ Input filename (and maybe -x suffix)
  -> Maybe Phase                -- ^ Optional stop phase
  -> Maybe FilePath             -- ^ original basename (if different from input filename)
  -> PipelineOutput             -- ^ Output filename
  -> Maybe ModLocation          -- ^ A ModLocation, if this is a Haskell module
  -> m (DynFlags, FilePath)     -- ^ (final flags, output filename)

runPipeline hooks stop_phase hsc_env0 input_fn mb_phase mb_basename output maybe_loc
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
    liftIO $ throwIO $ UsageError
      ("cannot compile this file to desired target: " ++ input_fn)

  -- this is a function which will be used to calculate output file names
  -- as we go along (we partially apply it to some of its inputs here)
  let get_output_fn = getOutputFilename stop_phase output basename

  -- Execute the pipeline...
  (dflags', output_fn, maybe_loc) <-
        pipeLoop hooks hsc_env start_phase stop_phase input_fn
                 basename suffix' get_output_fn maybe_loc

  -- Sometimes, a compilation phase doesn't actually generate any output
  -- (eg. the CPP phase when -fcpp is not turned on).  If we end on this
  -- stage, but we wanted to keep the output, then we have to explicitly
  -- copy the file, remembering to prepend a {-# LINE #-} pragma so that
  -- further compilation stages can tell what the original filename was.
  case output of
    Temporary ->
        return (dflags', output_fn)
    _other -> liftIO $
        do final_fn <- get_output_fn dflags' stop_phase maybe_loc
           when (final_fn /= output_fn) $ do
              let msg = ("Copying `" ++ output_fn ++"' to `" ++ final_fn ++ "'")
                  line_prag = Just ("{-# LINE 1 \"" ++ input_fn ++ "\" #-}\n")
              copyWithHeader dflags msg line_prag output_fn final_fn
           return (dflags', final_fn)


pipeLoop :: GhcMonad m =>
            PhaseImplementations m
         -> HscEnv -> Phase -> Phase
         -> FilePath  -> String -> Suffix
         -> (DynFlags -> Phase -> Maybe ModLocation -> IO FilePath)
         -> Maybe ModLocation
         -> m (DynFlags, FilePath, Maybe ModLocation)

pipeLoop hooks hsc_env phase stop_phase
         input_fn orig_basename orig_suff
         orig_get_output_fn maybe_loc

  | phase `eqPhase` stop_phase            -- All done
  = return (hsc_dflags hsc_env, input_fn, maybe_loc)

  | not (phase `happensBefore` stop_phase)
        -- Something has gone wrong.  We'll try to cover all the cases when
        -- this could happen, so if we reach here it is a panic.
        -- eg. it might happen if the -C flag is used on a source file that
        -- has {-# OPTIONS -fasm #-}.
  = panic ("pipeLoop: at phase " ++ show phase ++
           " but I wanted to stop at phase " ++ show stop_phase)

  | otherwise
  = do liftIO $ debugTraceMsg (hsc_dflags hsc_env) 4
                              (ptext (sLit "Running phase") <+> ppr phase)
       (next_phase, dflags', maybe_loc, output_fn)
          <- runPhase hooks phase stop_phase hsc_env orig_basename
                      orig_suff input_fn orig_get_output_fn maybe_loc
       let hsc_env' = hsc_env {hsc_dflags = dflags'}
       pipeLoop hooks hsc_env' next_phase stop_phase output_fn
                orig_basename orig_suff orig_get_output_fn maybe_loc

getOutputFilename
  :: Phase -> PipelineOutput -> String
  -> DynFlags -> Phase{-next phase-} -> Maybe ModLocation -> IO FilePath
getOutputFilename stop_phase output basename = func
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
      keep_raw_s = dopt Opt_KeepRawSFiles dflags
      keep_s     = dopt Opt_KeepSFiles dflags
      keep_bc    = dopt Opt_KeepLlvmFiles dflags

      myPhaseInputExt HCc    = hcsuf
      myPhaseInputExt StopLn = osuf
      myPhaseInputExt other  = phaseInputExt other

      is_last_phase = next_phase `eqPhase` stop_phase

      -- sometimes, we keep output from intermediate stages
      keep_this_output =
           case next_phase of
                   StopLn               -> True
                   Mangle  | keep_raw_s -> True
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

-- | Each phase in the pipeline returns the next phase to execute, and the
-- name of the file in which the output was placed.
--
-- We must do things dynamically this way, because we often don't know
-- what the rest of the phases will be until part-way through the
-- compilation: for example, an {-# OPTIONS -fasm #-} at the beginning
-- of a source file can change the latter stages of the pipeline from
-- taking the via-C route to using the native code generator.
--
runPhase :: GhcMonad m =>
            PhaseImplementations m
         -> Phase       -- ^ Do this phase first
         -> Phase       -- ^ Stop just before this phase
         -> HscEnv
         -> String      -- ^ basename of original input source
         -> String      -- ^ its extension
         -> FilePath    -- ^ name of file which contains the input to this phase.
         -> (DynFlags -> Phase -> Maybe ModLocation -> IO FilePath)
                        -- ^ how to calculate the output filename
         -> Maybe ModLocation           -- ^ the ModLocation, if we have one
         -> m (Phase,                   -- next phase
               DynFlags,                -- new dynamic flags
               Maybe ModLocation,       -- the ModLocation, if we have one
               FilePath)                -- output filename

        -- Invariant: the output filename always contains the output
        -- Interesting case: Hsc when there is no recompilation to do
        --                   Then the output filename is still a .o file

runPhase hooks (Unlit sf) _stop hsc_env _basename _suff input_fn get_output_fn maybe_loc
  = do let dflags = hsc_dflags hsc_env
       output_fn <- liftIO $ get_output_fn dflags (Cpp sf) maybe_loc
       let unlit_flags = getOpts dflags opt_L

       runUnlit hooks dflags unlit_flags input_fn output_fn

       return (Cpp sf, dflags, maybe_loc, output_fn)

runPhase hooks (Cpp sf) _stop hsc_env _basename _suff input_fn get_output_fn maybe_loc
  = do let dflags0 = hsc_dflags hsc_env
       src_opts <- liftIO $ getOptionsFromFile dflags0 input_fn
       (dflags1, unhandled_flags, warns)
           <- liftIO $ parseDynamicNoPackageFlags dflags0 src_opts
       checkProcessArgsResult unhandled_flags

       if not (xopt Opt_Cpp dflags1) then do
           -- we have to be careful to emit warnings only once.
           unless (dopt Opt_Pp dflags1) $ handleFlagWarnings dflags1 warns

           -- no need to preprocess CPP, just pass input file along
           -- to the next phase of the pipeline.
           return (HsPp sf, dflags1, maybe_loc, input_fn)
        else do
            output_fn <- liftIO $ get_output_fn dflags1 (HsPp sf) maybe_loc
            liftIO $ doCpp dflags1 True{-raw-} False{-no CC opts-} input_fn output_fn
            -- re-read the pragmas now that we've preprocessed the file
            -- See #2464,#3457
            src_opts <- liftIO $ getOptionsFromFile dflags0 output_fn
            (dflags2, unhandled_flags, warns)
                <- liftIO $ parseDynamicNoPackageFlags dflags0 src_opts
            unless (dopt Opt_Pp dflags2) $ handleFlagWarnings dflags2 warns
            -- the HsPp pass below will emit warnings
            checkProcessArgsResult unhandled_flags

            return (HsPp sf, dflags2, maybe_loc, output_fn)

runPhase hooks (HsPp sf) _stop hsc_env basename suff input_fn get_output_fn maybe_loc
  = do let dflags = hsc_dflags hsc_env
       if not (dopt Opt_Pp dflags) then
           -- no need to preprocess, just pass input file along
           -- to the next phase of the pipeline.
          return (Hsc sf, dflags, maybe_loc, input_fn)
        else do
            let hspp_opts = getOpts dflags opt_F
            let orig_fn = basename <.> suff
            output_fn <- liftIO $ get_output_fn dflags (Hsc sf) maybe_loc
            liftIO $ SysTools.runPp dflags
                           ( [ SysTools.Option     orig_fn
                             , SysTools.Option     input_fn
                             , SysTools.FileOption "" output_fn
                             ] ++
                             map SysTools.Option hspp_opts
                           )

            -- re-read pragmas now that we've parsed the file (see #3674)
            src_opts <- liftIO $ getOptionsFromFile dflags output_fn
            (dflags1, unhandled_flags, warns)
                <- liftIO $ parseDynamicNoPackageFlags dflags src_opts
            handleFlagWarnings dflags1 warns
            checkProcessArgsResult unhandled_flags

            return (Hsc sf, dflags1, maybe_loc, output_fn)

-- Hsc phase

-- Compilation of a single module, in "legacy" mode (_not_ under
-- the direction of the compilation manager).
runPhase hooks (Hsc src_flavour) stop hsc_env basename suff input_fn get_output_fn _maybe_loc
 = do   -- normal Hsc mode, not mkdependHS
        let dflags0 = hsc_dflags hsc_env

  -- we add the current directory (i.e. the directory in which
  -- the .hs files resides) to the include path, since this is
  -- what gcc does, and it's probably what you want.
        let current_dir = case takeDirectory basename of
                      "" -> "." -- XXX Hack
                      d -> d

            paths = includePaths dflags0
            dflags = dflags0 { includePaths = current_dir : paths }

  -- gather the imports and module name
        (hspp_buf,mod_name,imps,src_imps) <-
            case src_flavour of
                ExtCoreFile -> do  -- no explicit imports in ExtCore input.
                    m <- liftIO $ getCoreModuleName input_fn
                    return (Nothing, mkModuleName m, [], [])

                _           -> do
                    buf <- liftIO $ hGetStringBuffer input_fn
                    (src_imps,imps,L _ mod_name) <- getImports dflags buf input_fn (basename <.> suff)
                    return (Just buf, mod_name, imps, src_imps)

  -- Build a ModLocation to pass to hscMain.
  -- The source filename is rather irrelevant by now, but it's used
  -- by hscMain for messages.  hscMain also needs
  -- the .hi and .o filenames, and this is as good a way
  -- as any to generate them, and better than most. (e.g. takes
  -- into accout the -osuf flags)
        location1 <- liftIO $ mkHomeModLocation2 dflags mod_name basename suff

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


  -- Figure out if the source has changed, for recompilation avoidance.
  --
  -- Setting source_unchanged to True means that M.o seems
  -- to be up to date wrt M.hs; so no need to recompile unless imports have
  -- changed (which the compiler itself figures out).
  -- Setting source_unchanged to False tells the compiler that M.o is out of
  -- date wrt M.hs (or M.o doesn't exist) so we must recompile regardless.
        src_timestamp <- liftIO $ getModificationTime (basename <.> suff)

        let force_recomp = dopt Opt_ForceRecomp dflags
            hsc_lang = hscMaybeAdjustTarget dflags stop src_flavour (hscTarget dflags)
        source_unchanged <-
          if force_recomp || not (isStopLn stop)
                -- Set source_unchanged to False unconditionally if
                --      (a) recompilation checker is off, or
                --      (b) we aren't going all the way to .o file (e.g. ghc -S)
             then return False
                -- Otherwise look at file modification dates
             else do o_file_exists <- liftIO $ doesFileExist o_file
                     if not o_file_exists
                        then return False       -- Need to recompile
                        else do t2 <- liftIO $ getModificationTime o_file
                                if t2 > src_timestamp
                                  then return True
                                  else return False

  -- get the DynFlags
        let next_phase = hscNextPhase dflags src_flavour hsc_lang
        output_fn  <- liftIO $ get_output_fn dflags next_phase (Just location4)

        let dflags' = dflags { hscTarget = hsc_lang,
                               hscOutName = output_fn,
                               extCoreName = basename ++ ".hcr" }

        let hsc_env' = hsc_env {hsc_dflags = dflags'}

  -- Tell the finder cache about this module
        mod <- liftIO $ addHomeModuleToFinder hsc_env' mod_name location4

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
                                        ms_imps      = imps,
                                        ms_srcimps   = src_imps }

  -- run the compiler!
        result <- runHsCompiler hooks hsc_env' mod_summary source_unchanged
        -- liftIO $ putStrLn $ "Next phase = " ++ show next_phase

        case result of
          HscNoRecomp
              -> do liftIO $ SysTools.touch dflags' "Touching object file" o_file
                    -- The .o file must have a later modification date
                    -- than the source file (else we wouldn't be in HscNoRecomp)
                    -- but we touch it anyway, to keep 'make' happy (we think).
                    return (StopLn, dflags', Just location4, o_file)
          (HscRecomp hasStub _)
              -> do when hasStub $ do
                      error $ "stub files not yet supported"
                         -- do stub_o <- compileStub hsc_env' mod location4
                         --    liftIO $ consIORef v_Ld_inputs stub_o
                    -- In the case of hs-boot files, generate a dummy .o-boot
                    -- stamp file for the benefit of Make
                    when (isHsBoot src_flavour) $
                      liftIO $ SysTools.touch dflags' "Touching object file" o_file
                    return (next_phase, dflags', Just location4, output_fn)

runPhase hooks As _stop hsc_env _basename _suff input_fn get_output_fn maybe_loc
  = liftIO $
    do  let dflags = hsc_dflags hsc_env
        return (StopLn, dflags, maybe_loc, input_fn)
{-
    do  let dflags = hsc_dflags hsc_env
        let as_opts =  getOpts dflags opt_a
        let cmdline_include_paths = includePaths dflags

        output_fn <- get_output_fn dflags StopLn maybe_loc

        -- we create directories for the object file, because it
        -- might be a hierarchical module.
        createDirectoryHierarchy (takeDirectory output_fn)

        let (md_c_flags, _) = machdepCCOpts dflags
        SysTools.runAs dflags
                       (map SysTools.Option as_opts
                       ++ [ SysTools.Option ("-I" ++ p) | p <- cmdline_include_paths ]
                       ++ [ SysTools.Option "-c"
                          , SysTools.FileOption "" input_fn
                          , SysTools.Option "-o"
                          , SysTools.FileOption "" output_fn
                          ]
                       ++ map SysTools.Option md_c_flags)

        return (StopLn, dflags, maybe_loc, output_fn)
-}
-- warning suppression
runPhase hooks other _stop _dflags _basename _suff _input_fn _get_output_fn _maybe_loc =
   panic ("runPhase: don't know how to run phase " ++ show other)


-- Running CPP

doCpp :: DynFlags -> Bool -> Bool -> FilePath -> FilePath -> IO ()
doCpp dflags raw include_cc_opts input_fn output_fn = do
    let hscpp_opts = getOpts dflags opt_P
    let cmdline_include_paths = includePaths dflags

    pkg_include_dirs <- getPackageIncludePath dflags []
    let include_paths = foldr (\ x xs -> "-I" : x : xs) []
                          (cmdline_include_paths ++ pkg_include_dirs)

    let verb = getVerbFlag dflags

    let cc_opts
          | not include_cc_opts = []
          | otherwise           = (optc ++ md_c_flags)
                where
                      optc = getOpts dflags opt_c
                      (md_c_flags, _) = machdepCCOpts dflags

    let cpp_prog args | raw       = SysTools.runCpp dflags args
                      | otherwise = SysTools.runCc dflags (SysTools.Option "-E" : args)

    let target_defs =
          [ "-D" ++ HOST_OS     ++ "_BUILD_OS=1",
            "-D" ++ HOST_ARCH   ++ "_BUILD_ARCH=1",
            "-D" ++ TARGET_OS   ++ "_HOST_OS=1",
            "-D" ++ TARGET_ARCH ++ "_HOST_ARCH=1" ]
        -- remember, in code we *compile*, the HOST is the same our TARGET,
        -- and BUILD is the same as our HOST.

    cpp_prog       ([SysTools.Option verb]
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

hscMaybeAdjustTarget :: DynFlags -> Phase -> HscSource -> HscTarget -> HscTarget
hscMaybeAdjustTarget dflags stop _ current_hsc_lang = hsc_lang
 where
   keep_hc = dopt Opt_KeepHcFiles dflags
   hsc_lang
     -- don't change the lang if we're interpreting
     | current_hsc_lang == HscInterpreted = current_hsc_lang

     -- force -fvia-C if we are being asked for a .hc file
     | HCc <- stop = HscC
     | keep_hc     = HscC
     -- otherwise, stick to the plan
     | otherwise = current_hsc_lang

hscNextPhase :: DynFlags -> HscSource -> HscTarget -> Phase
hscNextPhase _ HsBootFile _        =  StopLn
hscNextPhase dflags _ hsc_lang =
  case hsc_lang of
        HscC -> HCc
        HscAsm | dopt Opt_SplitObjs dflags -> SplitMangle
               | otherwise -> As
        HscLlvm        -> LlvmOpt
        HscNothing     -> StopLn
        HscInterpreted -> StopLn
        _other         -> StopLn


defaultPhaseImplementations :: GhcMonad m => PhaseImplementations m
defaultPhaseImplementations = PhaseImplementations
  { runUnlit = defaultRunUnlit
  , runHsCompiler = defaultRunHsCompiler }

defaultRunUnlit ::
     DynFlags -> [String] -> InputFilePath -> OutputFilePath -> IO ()
defaultRunUnlit dflags unlit_flags input_fn output_fn = do
  let flags = map SysTools.Option unlit_flags ++
                  [ -- The -h option passes the file name for unlit to
                    -- put in a #line directive
                    SysTools.Option     "-h"
                    -- cpp interprets \b etc as escape sequences,
                    -- so we use / for filenames in pragmas
                  , SysTools.Option $ reslash Forwards $ normalise input_fn
                  , SysTools.FileOption "" input_fn
                  , SysTools.FileOption "" output_fn
                  ]
  SysTools.runUnlit dflags flags

defaultRunHsCompiler ::
     HscEnv -> ModSummary -> SourceUnchanged -> IO HscStatus
defaultRunHsCompiler hsc_env mod_summary source_unchanged = do
  hscCompileOneShot hsc_env mod_summary source_unchanged
                    Nothing       -- No iface
                    Nothing       -- No "module i of n" progress info

-- Utilities

ensureFileExists :: FilePath -> IO ()
ensureFileExists file = do
  exists <- doesFileExist file
  if (not exists) then
    throwIO (CmdLineError ("does not exist: " ++ file))
  else
    return ()
