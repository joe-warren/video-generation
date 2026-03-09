module Lib
    ( someFunc
    ) where

import qualified Data.Text.IO as T
import qualified CodeScene 
import qualified CodeBlock
import GenerateVideo (runBuildVideo, runWriteFrames)
import VideoProps 
import qualified ExampleObject
import qualified WaterfallScene

import Effectful.Reader.Static
import Effectful
import Data.Default (def)

videoProps :: VideoProps
videoProps = VideoProps 
    { _videoWidth = 1920
    , _videoHeight = 1080
    , _videoFPS = 25
    , _videoScratchDir = "output"
    , _videoOutputFile = "output.mp4"
    } 

run =
    runEff 
    . CodeBlock.runLoadCodeBlocks'
    . CodeScene.runHighlight
    . runReader videoProps
    . runBuildVideo
    . runWriteFrames

someFunc :: IO ()
someFunc = run $ do
    CodeScene.codeScene def 
        =<< CodeBlock.loadCodeBlock "src/ExampleObject.hs" "Intro"
    CodeScene.codeScene def  
        =<< CodeBlock.loadCodeBlock "src/ExampleObject.hs" "Profile"
    WaterfallScene.solidClip ExampleObject.spatula

