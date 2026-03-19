 module GenerateVideo
( generateVideo 
, FramePath 
, WriteFrames (..)
, BuildVideo (..)
, TrackOffset (..)
, getCurrentOffsetSeconds
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
import Control.Monad ((<=<), replicateM_, when)
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
    AddFrame :: Svg.Document -> BuildVideo m ()
    AddDuration :: Double -> Svg.Document -> BuildVideo m ()

type instance DispatchOf BuildVideo = Dynamic

data TrackOffset :: Effect where
    GetCurrentOffsetSeconds :: TrackOffset m Double

type instance DispatchOf TrackOffset = Dynamic

addSvgFrame :: (BuildVideo :> es) => Svg.Document -> Eff es ()
addSvgFrame = send . AddFrame 

addSvgDuration :: (BuildVideo :> es) => Double -> Svg.Document -> Eff es ()
addSvgDuration dur = send . AddDuration dur

getCurrentOffsetSeconds :: (TrackOffset :> es) =>  Eff es Double
getCurrentOffsetSeconds = send GetCurrentOffsetSeconds

runWriteFrames :: (IOE :> es, Reader VideoProps :> es) => Eff (WriteFrames : es) a -> Eff (es) a
runWriteFrames = do
    reinterpret (evalState (0::Integer)) $ \_ -> \case
        WriteSvgFrame document -> do
            (index::Integer) <- get
            vd <- ask
            let name = printf "%04d" index <> ".svg"
            let path = vd ^. videoScratchDir <> "/" <> name
            liftIO $ Svg.saveXmlFile path document
            modify (+1) 
            if (vd ^. videoConvertViaPng) 
                then do
                    let pngName = printf "%04d" index <> ".png"
                    let pngPath = vd ^. videoScratchDir <> "/" <> pngName
                    liftIO $ convertFile path pngPath
                    return $ FramePath pngName
                else return $ FramePath name
                
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

runBuildVideo :: (IOE :> es, Reader VideoProps :> es, WriteFrames :> es) => Eff (BuildVideo : es) a -> Eff (es) a
runBuildVideo eff = do
    vd <- ask
    let concatFileName = vd ^. videoScratchDir <> "/concat.txt"
    handle <- liftIO $ openFile concatFileName WriteMode
    let writeOneFrame fp = T.hPutStr handle (concatLine vd fp)
    res <- reinterpretWith (evalState (0::Double))eff $ \_ -> \case 
        AddFrame f -> liftIO . writeOneFrame =<< send (WriteSvgFrame f)
        AddDuration dur f -> do
            currentPos <- get
            let newPos = currentPos + (dur * fromIntegral (vd ^. videoFPS))
                (nFrames, posRemainder) =  properFraction newPos
            when (nFrames > 0) $ do 
                fp <- send $ WriteSvgFrame f
                liftIO $ replicateM_ nFrames (writeOneFrame fp)
            put posRemainder
    liftIO $ hClose handle
    let audioFile = 
            if (vd ^. videoGenerateAudio)
                then Just (vd ^. videoScratchDir <> "/audio.wav")
                else Nothing
    liftIO $ generateVideo concatFileName audioFile (vd ^. videoOutputFile)
    return res

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