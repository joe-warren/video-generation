module Lib
    ( someFunc
    ) where

import qualified Data.Text.IO as T
import qualified CodeScene 
import qualified CodeBlock
import GenerateVideo (runBuildVideo, runWriteFrames, addSvgDuration)
import VideoProps 
import qualified Transitions

import qualified ExampleObject
import qualified WaterfallScene
import qualified Waterfall as W

import Effectful.Reader.Static
import Effectful
import Data.Default (def)
import Control.Lens
import Linear
import GHC.Plugins (RdrName(Exact))
import Animate (animate)
import qualified SvgUtils
import Transitions (easeInOutSin)

videoProps :: VideoProps
videoProps = def

run =
    runEff 
    . CodeBlock.runLoadCodeBlocks'
    . CodeScene.runHighlight
    . runReader videoProps
    . runBuildVideo
    . runWriteFrames

easeInOutSoft :: Double -> Double
easeInOutSoft x 
    | x < fac = Transitions.easeInOutBack (x / fac)-- (easeInOutSin x + x) / 2 
    | otherwise = 1
    where fac = 0.9

someFunc :: IO ()
someFunc = run $ do
    let codeBlockOno blockname = 
            addSvgDuration 1.0
            =<< CodeScene.codeScene def 2.0
            =<< CodeBlock.loadCodeBlock "src/ExampleObject.hs" blockname

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
                =<< CodeBlock.loadCodeBlock "src/ExampleObject.hs" blockname
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
            
    codeBlockOno "Intro"
    codeBlockOno "Imports"
    codeBlockWithDiagram "Profile"
        (ExampleObject.bladeProfile 
            & W.pathDiagram W.OutLine W.Visible
            & W.uScale2D (1/80)
            & WaterfallScene.centerDiagram
        )
        
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

