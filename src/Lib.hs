module Lib
    ( someFunc
    ) where

import qualified Data.Text.IO as T
import qualified CodeScene 
import ConcatFileVideo (generateVideo)
import VideoData 
import ExampleObject (csgExample)
import qualified WaterfallScene

videoData :: VideoData
videoData = VideoData 
    { videoWidth = 1920
    , videoHeight = 1080
    , scratchDir = "output"
    } 

someFunc :: IO ()
someFunc = generateVideo "output" "output.mp4" 
            =<< WaterfallScene.solidClip videoData csgExample
            -- =<< CodeScene.highlightAndSave videoData
            -- =<< T.readFile "src/Lib.hs"
