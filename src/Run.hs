module Run (run) where

import qualified CodeScene
import qualified CodeBlock
import GenerateVideo (runBuildVideo, runWriteFrames, addSvgDuration, runTrackOffset)
import GenerateAudio (runBuildAudio, logBuildAudioError)
import qualified ImageScene
import VideoProps

import Effectful.Reader.Static
import Effectful

run videoProps =
    runEff
    . logBuildAudioError
    . CodeBlock.logCodeBlockError
    . CodeBlock.runLoadCodeBlocks
    . CodeScene.runHighlight
    . runReader videoProps
    . ImageScene.runLoadImages
    . runWriteFrames
    . runBuildVideo
    . runTrackOffset
    . runBuildAudio