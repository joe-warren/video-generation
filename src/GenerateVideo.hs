 module GenerateVideo
( generateVideo 
, FramePath 
, WriteFrames (..)
, BuildVideo (..)
, TrackOffset (..)
, getCurrentOffsetSeconds
, addSvgFrame
, addSvgDuration
, addImageDuration
, runBuildVideo
, runWriteFrames
, runTrackOffset
) where

import VideoProps

import Numeric (showFFloat)
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified System.Process as Process
import qualified Graphics.Svg as Svg
import SvgUtils (imageFile)
import Effectful
import Effectful.Dispatch.Dynamic
import Control.Monad ((<=<), replicateM_, when)
import Effectful.Reader.Static
import Text.Printf (printf)
import System.IO (openFile, IOMode (WriteMode), hClose)
import System.FilePath (takeExtension)
import Effectful.State.Static.Local
import Control.Lens
import System.Directory (copyFile)

data FramePath = FramePath { framePathFilePath :: FilePath } 

data WriteFrames :: Effect where
    WriteSvgFrame :: Svg.Document -> WriteFrames m FramePath
    WriteFileFrame :: FilePath -> WriteFrames m FramePath

type instance DispatchOf WriteFrames = Dynamic

writeFrame :: (WriteFrames :> es) => Frame -> Eff es FramePath
writeFrame (SvgFrame doc) = send $ WriteSvgFrame doc
writeFrame (FileFrame path) = send $ WriteFileFrame path

data Frame = SvgFrame Svg.Document | FileFrame FilePath

data BuildVideo :: Effect where
    AddFrame :: Frame -> BuildVideo m ()
    AddDuration :: Double -> Frame -> BuildVideo m ()

type instance DispatchOf BuildVideo = Dynamic

data TrackOffset :: Effect where
    GetCurrentOffsetSeconds :: TrackOffset m Double

type instance DispatchOf TrackOffset = Dynamic


addSvgFrame :: (BuildVideo :> es) => Svg.Document -> Eff es ()
addSvgFrame = send . AddFrame . SvgFrame

addSvgDuration :: (BuildVideo :> es) => Double -> Svg.Document -> Eff es ()
addSvgDuration dur = send . AddDuration dur . SvgFrame

addImageDuration :: (BuildVideo :> es) => Double -> FilePath -> Eff es ()
addImageDuration dur = send . AddDuration dur . FileFrame

getCurrentOffsetSeconds :: (TrackOffset :> es) =>  Eff es Double
getCurrentOffsetSeconds = send GetCurrentOffsetSeconds

runWriteFrames :: (IOE :> es, Reader VideoProps :> es) => Eff (WriteFrames : es) a -> Eff (es) a
runWriteFrames = do
    reinterpret (evalState (0::Integer)) $ \_ -> \action -> do
            (index::Integer) <- get
            vd <- ask
            
            modify (+1) 
            
            let writeSVG document = do
                    let name = printf "%04d" index <> ".svg"
                        path = vd ^. videoScratchDir <> "/" <> name
                    Svg.saveXmlFile path document
                    if (vd ^. videoConvertViaPng) 
                        then do
                            let pngName = printf "%04d" index <> ".png"
                            let pngPath = vd ^. videoScratchDir <> "/" <> pngName
                            convertFile path pngPath
                            return $ FramePath pngName
                        else return $ FramePath name

            case action of 
                WriteFileFrame file -> do
                    let extension = takeExtension file
                        name = printf "%04d" index <> extension
                        path = vd ^. videoScratchDir <> "/" <> name

                    liftIO $ copyFile file path
                    liftIO . writeSVG $ imageFile vd name
                WriteSvgFrame document -> liftIO $ writeSVG document
                
runTrackOffset:: (BuildVideo :> es, Reader VideoProps :> es) => Eff (TrackOffset : es) a -> Eff (es) a
runTrackOffset = 
    reinterpret (evalState (0::Double) 
        . interpose (\_ -> \case 
            AddFrame f -> do
                vd <- ask
                modify (+ (1 / fromInteger (vd ^. videoFPS)))
                send (AddFrame f)
            AddDuration dur f -> do
                modify (+ dur)
                send (AddDuration dur f)
        )
    ) $ \_ -> \case 
            GetCurrentOffsetSeconds -> get
        

concatLine :: VideoProps -> FramePath -> Text
concatLine vd (FramePath path)= 
    let showFloat f = T.pack $ showFFloat Nothing f [] 
    in "file '" <> T.pack path <> "'\n" <>
        "duration " <> showFloat (1 / (fromInteger $ vd ^. videoFPS)) <> "\n"

runBuildVideoFfmpeg :: (IOE :> es, Reader VideoProps :> es, WriteFrames :> es) => Eff (BuildVideo : es) a -> Eff (es) a
runBuildVideoFfmpeg eff = do
    vd <- ask
    let concatFileName = vd ^. videoScratchDir <> "/concat.txt"
    handle <- liftIO $ openFile concatFileName WriteMode
    let writeOneFrame fp = T.hPutStr handle (concatLine vd fp)
    res <- reinterpretWith (evalState (0::Double))eff $ \_ -> \case 
        AddFrame f -> liftIO . writeOneFrame =<< writeFrame f
        AddDuration dur f -> do
            currentPos <- get
            let newPos = currentPos + (dur * fromIntegral (vd ^. videoFPS))
                (nFrames, posRemainder) =  properFraction newPos
            when (nFrames > 0) $ do 
                fp <- writeFrame f
                liftIO $ replicateM_ nFrames (writeOneFrame fp)
            put posRemainder
    liftIO $ hClose handle
    let audioFile = 
            if (vd ^. videoGenerateAudio)
                then Just (vd ^. videoScratchDir <> "/audio.wav")
                else Nothing
    liftIO $ generateVideo concatFileName audioFile (vd ^. videoOutputFile)
    return res

runBuildVideoNoop :: Eff (BuildVideo : es) a -> Eff es a
runBuildVideoNoop = interpret $ \_ -> \case
    AddFrame _ -> pure ()
    AddDuration _ _ -> pure ()
    
runBuildVideo :: (IOE :> es, Reader VideoProps :> es, WriteFrames :> es) => Eff (BuildVideo : es) a -> Eff (es) a
runBuildVideo eff = do
    vd <- ask
    if vd ^. videoGenerateVideo 
        then runBuildVideoFfmpeg eff
        else runBuildVideoNoop eff

convertFile :: FilePath -> FilePath -> IO ()
convertFile inputFile outputFile = do
    Process.callProcess "rsvg-convert"
        [ "--output=" <> outputFile
        , inputFile
        ]

generateVideo :: FilePath -> Maybe FilePath -> FilePath -> IO ()
generateVideo concatFile audioFile outputFile = 
    let audioCommands = case audioFile of 
            Nothing -> []
            Just f -> 
                [ "-i", f
                , "-shortest"
                , "-map", "0:v:0"
                , "-map", "1:a:0"
                ]
    in Process.callProcess "ffmpeg"
        (["-y"
        , "-f", "concat"
        , "-i", concatFile
        ] <> audioCommands <>
        [ "-vsync", "vfr"
        , "-pix_fmt", "yuv420p"
        , outputFile
        ])