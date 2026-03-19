module GenerateAudio (
    setTidalPattern,
    setTidalOp,
    BuildAudioError(..),
    runBuildAudio,
    logBuildAudioError
) where

import VideoProps
import GenerateVideo (TrackOffset, getCurrentOffsetSeconds)

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.Reader.Static
import Effectful.Writer.Static.Local
import Effectful.State.Static.Local
import qualified Sound.Tidal.Safe.Boot as Tidal
import qualified Sound.Tidal.Safe.Context as Tidal
import Sound.Tidal.Boot (mkTidal)
import Control.Monad (foldM_, forever)
import Control.Concurrent (threadDelay, forkIO)
import qualified System.Process as Process
import System.IO (Handle, hGetLine, hIsEOF)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Lens

data BuildAudioError =
    SuperDirtStartupFailed
    deriving Show

data BuildAudio :: Effect where
    SetTidalOp :: Tidal.Op () -> BuildAudio m ()

type instance DispatchOf BuildAudio = Dynamic

setTidalPattern :: (BuildAudio :> es) => Tidal.ControlPattern -> Eff es ()
setTidalPattern = send . SetTidalOp . Tidal.d1

setTidalOp :: (BuildAudio :> es) => Tidal.Op () -> Eff es ()
setTidalOp = send . SetTidalOp

runSuperdirt :: VideoProps -> IO (Handle, Handle, Process.ProcessHandle)
runSuperdirt props = do
    (Just stdinHandle, Just stdoutHandle, _, processHandle) <- Process.createProcess $
        Process.CreateProcess
            (Process.RawCommand
                "pw-jack" -- run superdirt within pipewire jack emulation
                ["sclang", "superdirt_startup.scd"] -- command line args
            )
            Nothing -- no cwd
            Nothing -- inherit environment
            Process.CreatePipe -- create a handle, so we can write commands to superdirt
            Process.CreatePipe -- capture stdout so we can wait for READY
            Process.Inherit -- use existing stderr
            False  -- don't close fds
            False -- don't create a group
            False -- Superdirt isn't King Cnut
            False -- don't detatch console
            False -- don't create new console
            False -- no new session
            Nothing -- no child group
            Nothing -- no child user
            False -- no use process jobs

    return (stdinHandle, stdoutHandle, processHandle)

waitForReady :: (IOE :> es, Error BuildAudioError :> es) => Handle -> Eff es ()
waitForReady h = do
    eof <- liftIO $ hIsEOF h
    if eof
        then throwError SuperDirtStartupFailed
        else do
            line <- liftIO $ hGetLine h
            liftIO $ putStrLn line
            if line == "SUPERDIRT TAPE ROLLING"
                then return ()
                else waitForReady h

forwardOutput :: Handle -> IO ()
forwardOutput h = forever $ do
    eof <- hIsEOF h
    if eof
        then threadDelay 100000
        else hGetLine h >>= putStrLn

runBuildAudioNoOp :: Eff (BuildAudio : es) a -> Eff es a
runBuildAudioNoOp = interpret $ \_ -> \case
    SetTidalOp _ -> pure ()

runBuildAudioTidal :: (IOE :> es, Reader VideoProps :> es, TrackOffset :> es, Error BuildAudioError :> es) => Eff (BuildAudio : es) a -> Eff es a
runBuildAudioTidal eff = do
    (res, patterns:: [(Double, Tidal.Op ())]) <-
        reinterpretWith (runWriter) eff $ \_ -> \case
            SetTidalOp op -> do
                offset <- getCurrentOffsetSeconds
                tell [(offset, op)]

    finalOffset <- getCurrentOffsetSeconds
    let finalAction = [(finalOffset, Tidal.hush )]
    let initialAction = [(0, pure ())]

    vd <- ask
    (stdinHandle, stdoutHandle, processHandle) <- liftIO $ runSuperdirt vd
    waitForReady stdoutHandle
    liftIO $ forkIO $ forwardOutput stdoutHandle
    tidalInst <- liftIO $ mkTidal

    let allPatterns = (initialAction <> patterns <> finalAction)

    liftIO $ print $ fst <$> allPatterns
    let doPattern curOffset (offset, op) = do
            threadDelay (floor (1000000 * (offset - curOffset)))
            putStrLn $ "Tidal Event at : " <> show offset <> " Seconds"
            Tidal.exec tidalInst op
            return offset
    liftIO $ foldM_ doPattern 0 allPatterns

    liftIO $ Process.terminateProcess processHandle

    return res

runBuildAudio :: (IOE :> es, Reader VideoProps :> es, TrackOffset :> es, Error BuildAudioError :> es) => Eff (BuildAudio : es) a -> Eff es a
runBuildAudio eff = do
    vd <- ask
    if vd ^. videoGenerateAudio
        then runBuildAudioTidal eff
        else runBuildAudioNoOp eff

logBuildAudioError :: (IOE :> es) => Eff (Error BuildAudioError : es) () -> Eff es ()
logBuildAudioError =
    let handler callstack e = liftIO $ do
            putStrLn "BuildAudio Error"
            print e
            print callstack
    in runErrorWith handler