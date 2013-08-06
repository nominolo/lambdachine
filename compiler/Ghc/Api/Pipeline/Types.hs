module Ghc.Api.Pipeline.Types
  ( Source(..)
  , PhaseImplementations(..)
  , Phase(..), InputFilePath, OutputFilePath, SourceUnchanged
  , ModSummary, HscEnv, HscStatus, DynFlags
  )
where

import System.FilePath
import DriverPhases ( Phase(..) )
import DynFlags ( DynFlags )
import HscTypes ( HscEnv, ModSummary )
import HscMain ( HscStatus )

data Source = Source
  { sourceFileName :: !FilePath
  , sourceOptStopPhase :: Maybe Phase
  }

data PhaseImplementations = PhaseImplementations
  { runUnlit :: HscEnv -> DynFlags -> [String]
             -> InputFilePath -> OutputFilePath
             -> IO ()
    -- ^ How to run the @unlit@ program.  The arguments are;
    --
    --  * The DynFlags from the current session.
    --  * The arguments to the @unlit@ program.
    --  * The name of the input file.
    --  * The name of the output file.
    --
  , runHsCompiler :: HscEnv -> ModSummary -> SourceUnchanged
                  -> IO HscStatus }

type InputFilePath = FilePath
type OutputFilePath = FilePath
type SourceUnchanged = Bool
