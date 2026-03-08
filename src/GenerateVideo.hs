 module GenerateVideo
( generateVideo 
, FramePath 
, WriteFrames (..)
, BuildVideo (..)
, addSvgFrame
, addSvgDuration
, runBuildVideo
, runWriteFrames
) where

import qualified VideoData as VideoData
import VideoData (VideoData)

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

data FramePath = FramePath { framePathFilePath :: FilePath } 
data WriteFrames :: Effect where
    WriteSvgFrame :: Svg.Document -> WriteFrames m FramePath

type instance DispatchOf WriteFrames = Dynamic

data BuildVideo :: Effect where
    AddFrame :: FramePath -> BuildVideo m ()
    AddDuration :: Double -> FramePath -> BuildVideo m ()

type instance DispatchOf BuildVideo = Dynamic

addSvgFrame :: (WriteFrames :> es, BuildVideo :> es) => Svg.Document -> Eff es ()
addSvgFrame = send . AddFrame <=< send . WriteSvgFrame 

addSvgDuration :: (WriteFrames :> es, BuildVideo :> es) => Double -> Svg.Document -> Eff es ()
addSvgDuration dur = send . AddDuration dur <=< send . WriteSvgFrame 

runWriteFrames :: (IOE :> es, Reader VideoData :> es) => Eff (WriteFrames : es) a -> Eff (es) a
runWriteFrames eff = do
    ref <- liftIO $ newIORef (0:: Integer)
    interpretWith eff $ \_ -> \case
        WriteSvgFrame document -> do
            index <- liftIO $ readIORef ref
            vd <- ask
            let name = printf "%04d" index <> ".svg"
            let path = VideoData.scratchDir vd <> "/" <> name
            liftIO $ Svg.saveXmlFile path document
            liftIO $ modifyIORef ref (+1) 
            return $ FramePath name

concatLine :: VideoData -> FramePath -> Text
concatLine vd (FramePath path)= 
    let showFloat f = T.pack $ showFFloat Nothing f [] 
    in "file '" <> T.pack path <> "'\n" <>
        "duration " <> showFloat (1 / (fromInteger . VideoData.videoFPS $ vd)) <> "\n"

runBuildVideo :: (IOE :> es, Reader VideoData :> es) => Eff (BuildVideo : es) a -> Eff (es) a
runBuildVideo eff = do
    posRef <- liftIO $ newIORef (0 :: Double)
    vd <- ask
    let concatFileName = VideoData.scratchDir vd <> "/concat.txt"
    handle <- liftIO $ openFile concatFileName WriteMode
    let writeOneFrame fp = T.hPutStr handle (concatLine vd fp)
    res <- interpretWith eff $ \_ -> \case 
        AddFrame f -> liftIO $ writeOneFrame f
        AddDuration dur f -> liftIO $ do
            currentPos <- readIORef $ posRef
            let newPos = currentPos + (dur * fromIntegral (VideoData.videoFPS vd))
                (nFrames, posRemainder) =  properFraction newPos
            replicateM_ nFrames (writeOneFrame f)
            writeIORef posRef posRemainder
    liftIO $ hClose handle
    liftIO $ generateVideo concatFileName (VideoData.videoOutputFile vd)
    return res

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