module Main
    ( main
    ) where

import qualified Data.Text.IO as T
import qualified CodeScene 
import qualified CodeBlock
import GenerateVideo (runBuildVideo, runWriteFrames, addSvgDuration, runTrackOffset)
import VideoProps 
import qualified Transitions
import qualified Run

import qualified ExampleObject
import qualified ExampleAudio
import qualified WaterfallScene
import qualified Waterfall as W

import qualified Skylighting as Sky

import Effectful.Reader.Static
import Effectful
import Data.Default (def)
import Control.Lens
import Linear
import Animate (animate)
import qualified SvgUtils
import Transitions (easeInOutSin)
import GenerateAudio (runBuildAudio, setTidalPattern, setTidalCPS)

videoProps :: VideoProps
videoProps = def

easeInOutSoft :: Double -> Double
easeInOutSoft x 
    | x < fac = Transitions.easeInOutBack (x / fac)-- (easeInOutSin x + x) / 2 
    | otherwise = 1
    where fac = 0.9

main :: IO ()
main = Run.run videoProps $ do
    let codeBlockOno' srcfile props blockname = 
            addSvgDuration 1.0
            =<< CodeScene.codeScene props 2.0
            =<< CodeBlock.loadCodeBlock srcfile blockname

    let codeBlockOno = codeBlockOno' "spatula/ExampleObject.hs" def

    let center solid = 
            case W.axisAlignedBoundingBox solid of
                Just (lo, hi) -> W.translate ((lo + hi) ^* (-0.5)) solid
                Nothing -> solid

    let showRotating' axis scale angle solid t = solid
            & W.uScale scale 
            & W.rotate axis (angle * t)
            & W.solidDiagram (V3 2 2 1)
        showRotating = showRotating' (unit _z)

    let codeBlockWithFade blockname nextScene = do
            fullCode <- CodeScene.codeScene def 2.0
                =<< CodeBlock.loadCodeBlock "spatula/ExampleObject.hs" blockname
            addSvgDuration 1.5 fullCode

            fadedCode <- animate 0.5 
                ( SvgUtils.addBackground SvgUtils.white 
                . SvgUtils.makeOpaque fullCode 
                . (+ 1.0)
                . (* (-0.925))
                . easeInOutSin
                )

            nextScene fadedCode


    let codeBlockWithDiagram blockname diagram = 
            codeBlockWithFade blockname $ \background -> 
                WaterfallScene.stillClipWithBackground 2 background diagram

    let codeBlockWithObject blockname object = do
            codeBlockWithFade blockname $ \background -> 
                WaterfallScene.animatedClipWithBackground 5 background $ 
                    showRotating (1/150) (2*pi) (center object)  
                    . easeInOutSoft

    let audioCodeBlockParams = 
            def
            & CodeScene.codeSceneStyle .~ Sky.breezeDark
            
    let codeBlockAudioOno = codeBlockOno' "spatula/ExampleAudio.hs" audioCodeBlockParams

    let codeBlockTidalAudio blockname tidalEffect = do
            still <- CodeScene.codeScene audioCodeBlockParams 2.0
                =<< CodeBlock.loadCodeBlock "spatula/ExampleAudio.hs" blockname
            _ <- tidalEffect
            addSvgDuration 1.0 still

    setTidalCPS (120/60/4)

    codeBlockOno "Intro"

    codeBlockAudioOno "Audio Intro"
    codeBlockTidalAudio "Play Intro" $
        setTidalPattern ExampleAudio.intro

    codeBlockOno "Imports"
    codeBlockWithDiagram "Profile"
        (ExampleObject.bladeProfile 
            & W.pathDiagram W.OutLine W.Visible
            & W.uScale2D (1/80)
            & WaterfallScene.centerDiagram
        )

 
    codeBlockAudioOno "Verse 1"
    codeBlockTidalAudio "Play Verse 1" $
        setTidalPattern ExampleAudio.verse1
        
    codeBlockWithObject "Sharp Blade" ExampleObject.sharpBlade

    codeBlockWithObject "Blade" ExampleObject.blade

    codeBlockWithDiagram "Slot Profile"
        (ExampleObject.slotProfile 
            & W.pathDiagram W.OutLine W.Visible
            & W.uScale2D (1/80)
            & WaterfallScene.centerDiagram
        )

    codeBlockWithObject "Slots" ExampleObject.slots 

    codeBlockWithObject "Slotted Blade" ExampleObject.slottedBlade

    codeBlockAudioOno "Verse 2"
    codeBlockTidalAudio "Play Verse 2" $
        setTidalPattern ExampleAudio.verse2

    codeBlockOno "Handle Params"
    codeBlockOno "Handle Path"
    codeBlockWithObject "Handle" ExampleObject.handle

    codeBlockWithObject "Grip" ExampleObject.grip

    codeBlockWithObject "Handle And Grip" ExampleObject.handleWithGrip

    codeBlockWithObject "Hole" ExampleObject.handleWithHole
        
    codeBlockOno "Negative Mask" 
    codeBlockOno "Spatula"

    WaterfallScene.animatedClip 8 $ showRotating (1/150) (2*pi)
            ( ExampleObject.spatula 
                & center
            ) . easeInOutSoft
            
    WaterfallScene.animatedClip 8 $ showRotating' (unit _y) (1/150) (2*pi)
            ( ExampleObject.spatula 
                & center
            ) . easeInOutSoft

    WaterfallScene.animatedClip 8 $ showRotating' (unit _x) (1/150) (2*pi)
            ( ExampleObject.spatula 
                & center
            ) . easeInOutSoft

--}