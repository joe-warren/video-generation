module Lib
    ( someFunc
    ) where

import qualified Data.Text.IO as T
import qualified CodeScene 
import GenerateVideo (runBuildVideo, runWriteFrames)
import VideoData 
import ExampleObject (csgExample)
import qualified WaterfallScene

import Effectful.Reader.Static
import Effectful

import qualified Eval (eval)

videoData :: VideoData
videoData = VideoData 
    { videoWidth = 1920
    , videoHeight = 1080
    , videoFPS = 25
    , scratchDir = "output"
    , videoOutputFile = "output.mp4"
    } 

someFunc :: IO ()
someFunc = runEff . CodeScene.runHighlight . runReader videoData . runBuildVideo . runWriteFrames $ do
            WaterfallScene.solidClip csgExample
            CodeScene.highlightAndSave "hs" =<< liftIO (T.readFile "src/ExampleObject.hs")

    -- Eval.eval "test.hs" "Main.value"

