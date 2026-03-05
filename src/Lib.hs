module Lib
    ( someFunc
    ) where

import qualified Data.Text.IO as T
import qualified CodeScene 
import ConcatFileVideo (generateVideo)
import VideoData 
import ExampleObject (csgExample)
import qualified WaterfallScene

import qualified Eval (eval)

videoData :: VideoData
videoData = VideoData 
    { videoWidth = 1920
    , videoHeight = 1080
    , scratchDir = "output"
    } 

someFunc :: IO ()
someFunc = Eval.eval "test.hs" "Main.value"
    {--
        generateVideo "output" "output.mp4" 
            -- =<< WaterfallScene.solidClip videoData csgExample
            =<< CodeScene.highlightAndSave videoData
            =<< T.readFile "src/ExampleObject.hs"

    --}
