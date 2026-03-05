module Eval 
( eval
) where

import Data.Text (Text)
import qualified Data.Text as T 

import GHC
import GHC.Driver.Config.Diagnostic
import GHC.Driver.Errors
import GHC.Driver.Errors.Types
import GHC.Driver.Monad
import GHC.Plugins
import GHC.Runtime.Interpreter
import GHC.Utils.Exception

import Unsafe.Coerce (unsafeCoerce)
import GHC.Paths (libdir)

eval :: FilePath -> Text -> IO ()
eval filepath input = do
  defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
    runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        -- If we want to make GHC interpret our code on the fly, we
        -- ought to set those two flags, otherwise we
        -- wouldn't be able to use 'setContext' below
        setSessionDynFlags $ dflags 
            { ghcLink   = LinkInMemory
            , ghcMode = CompManager
            , verbosity = 1
            , backend = interpreterBackend 
            }
        setTargets =<< sequence [guessTarget filepath Nothing Nothing]
        load LoadAllTargets
        -- Bringing the module into the context
        setContext [IIModule $ mkModuleName "Main"]
        -- evaluating and running an action
        act <- unsafeCoerce <$> compileExpr (T.unpack input) 
        liftIO $ putStrLn act