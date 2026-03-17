 module GenerateVideo
( generateVideo 
, FramePath 
, WriteFrames (..)
, BuildVideo (..)
, addSvgFrame
, addSvgDuration
, runBuildVideo
, runWriteFrames
, runTrackOffset
, getCurrentOffsetSeconds
) where

import VideoProps

import Numeric (showFFloat)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified System.Process as Process
import qualified Graphics.Svg as Svg
import Effectful
import Effectful.Dispatch.Dynamic
import Control.Monad ((<=<), replicateM_)
import Data.IORef (newIORef, readIORef, modifyIORef, writeIORef)
import Effectful.Reader.Static
import Text.Printf (printf)
import System.IO (openFile, IOMode (WriteMode), hClose)
import Effectful.State.Static.Local
import Control.Lens

data FramePath = FramePath { framePathFilePath :: FilePath } 

data WriteFrames :: Effect where
    WriteSvgFrame :: Svg.Document -> WriteFrames m FramePath

type instance DispatchOf WriteFrames = Dynamic

data BuildVideo :: Effect where
    AddFrame :: FramePath -> BuildVideo m ()
    AddDuration :: Double -> FramePath -> BuildVideo m ()

type instance DispatchOf BuildVideo = Dynamic

data TrackOffset :: Effect where
    GetCurrentOffsetSeconds :: TrackOffset m Double

type instance DispatchOf TrackOffset = Dynamic

addSvgFrame :: (WriteFrames :> es, BuildVideo :> es) => Svg.Document -> Eff es ()
addSvgFrame = send . AddFrame <=< send . WriteSvgFrame 

addSvgDuration :: (WriteFrames :> es, BuildVideo :> es) => Double -> Svg.Document -> Eff es ()
addSvgDuration dur = send . AddDuration dur <=< send . WriteSvgFrame 

getCurrentOffsetSeconds :: (TrackOffset :> es) =>  Eff es Double
getCurrentOffsetSeconds = send GetCurrentOffsetSeconds

runWriteFrames :: (IOE :> es, Reader VideoProps :> es) => Eff (WriteFrames : es) a -> Eff (es) a
runWriteFrames eff = do
    ref <- liftIO $ newIORef (0:: Integer)
    interpretWith eff $ \_ -> \case
        WriteSvgFrame document -> do
            index <- liftIO $ readIORef ref
            vd <- ask
            let name = printf "%04d" index <> ".svg"
            let path = vd ^. videoScratchDir <> "/" <> name
            liftIO $ Svg.saveXmlFile path document
            liftIO $ modifyIORef ref (+1) 
            if (vd ^. videoConvertViaPng) 
                then do
                    let pngName = printf "%04d" index <> ".png"
                    let pngPath = vd ^. videoScratchDir <> "/" <> pngName
                    liftIO $ convertFile path pngPath
                    return $ FramePath pngName
                else return $ FramePath name

                
runTrackOffset:: (WriteFrames :> es, Reader VideoProps :> es) => Eff (TrackOffset : es) a -> Eff (es) a
runTrackOffset = 
    reinterpret (evalState (0::Integer) 
        . interpose (\_ -> \case 
            WriteSvgFrame doc -> do
                modify (+1)
                send (WriteSvgFrame doc)
        )
    ) $ \_ -> \case 
            GetCurrentOffsetSeconds -> do 
                frames <- get
                vd <- ask
                return (fromInteger frames / fromInteger (vd ^. videoFPS))
        

concatLine :: VideoProps -> FramePath -> Text
concatLine vd (FramePath path)= 
    let showFloat f = T.pack $ showFFloat Nothing f [] 
    in "file '" <> T.pack path <> "'\n" <>
        "duration " <> showFloat (1 / (fromInteger $ vd ^. videoFPS)) <> "\n"

runBuildVideo :: (IOE :> es, Reader VideoProps :> es) => Eff (BuildVideo : es) a -> Eff (es) a
runBuildVideo eff = do
    posRef <- liftIO $ newIORef (0 :: Double)
    offsetRef <- liftIO $ newIORef (0 :: Double)
    vd <- ask
    let concatFileName = vd ^. videoScratchDir <> "/concat.txt"
    handle <- liftIO $ openFile concatFileName WriteMode
    let writeOneFrame fp = T.hPutStr handle (concatLine vd fp)
    res <- interpretWith eff $ \_ -> \case 
        AddFrame f -> liftIO $ writeOneFrame f
        AddDuration dur f -> liftIO $ do
            currentPos <- readIORef $ posRef
            let newPos = currentPos + (dur * fromIntegral (vd ^. videoFPS))
                (nFrames, posRemainder) =  properFraction newPos
            replicateM_ nFrames (writeOneFrame f)
            writeIORef posRef posRemainder
    liftIO $ hClose handle
    liftIO $ generateVideo concatFileName (vd ^. videoOutputFile)
    return res

convertFile :: FilePath -> FilePath -> IO ()
convertFile inputFile outputFile = do
    Process.callProcess "rsvg-convert"
        [ "--output=" <> outputFile
        , inputFile
        ]

generateVideo :: FilePath -> FilePath -> IO ()
generateVideo concatFile outputFile = do
    Process.callProcess "ffmpeg"
        [ "-y"
        , "-f", "concat"
        , "-i", concatFile
        , "-vsync", "vfr"
        , "-pix_fmt", "yuv420p"
        , outputFile
        ]