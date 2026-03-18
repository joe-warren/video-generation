module Run (run) where
    
import qualified CodeScene 
import qualified CodeBlock
import GenerateVideo (runBuildVideo, runWriteFrames, addSvgDuration, runTrackOffset)
import GenerateAudio (runBuildAudio, setTidalPattern)
import VideoProps 

import Effectful.Reader.Static
import Effectful

run videoProps =
    runEff
    . CodeBlock.runLoadCodeBlocks'
    . CodeScene.runHighlight
    . runReader videoProps
    . runWriteFrames
    . runBuildVideo
    . runTrackOffset
    . runBuildAudio