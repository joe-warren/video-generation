module Main
    ( main
    ) where

import qualified CodeScene 
import qualified CodeBlock
import VideoProps 
import qualified Transitions
import qualified Run

import qualified Object
import qualified Audio
import qualified WaterfallScene
import qualified Waterfall as W

import qualified Skylighting as Sky

import Data.Default (def)
import Control.Lens
import Linear
import Animate (animate)
import qualified SvgUtils
import Transitions (easeInOutSin)
import qualified Sound.Tidal.Safe.Boot as Tidal
import Sound.Tidal.Boot (silence)
import GenerateAudio (setTidalPattern, setTidalOp)
import GenerateVideo (addSvgDuration)
import CodeScene (codeScene)
import Control.Monad (join)
import qualified Codec.Picture as JP

videoProps :: VideoProps
videoProps = def
   -- & videoGenerateAudio .~ False
   -- & videoGenerateVideo .~ False

easeInOutStay :: Double -> Double
easeInOutStay x 
    | x < fac = Transitions.easeInOutBack (x / fac)
    | otherwise = 1
    where fac = 0.9

main :: IO ()
main = Run.run videoProps $ do
    let codeBlockOno' srcfile props blockname = 
            addSvgDuration 0.8
            =<< CodeScene.codeScene props 1.4
            =<< CodeBlock.loadCodeBlock srcfile blockname

    let codeBlockOno = codeBlockOno' "spatula/Object.hs" def

    let centerBasedOn target solid = 
            case W.axisAlignedBoundingBox target of
                Just (lo, hi) -> W.translate ((lo + hi) ^* (-0.5)) solid
                Nothing -> solid
        center = join centerBasedOn

    let toDiagram =
            W.solidDiagram (V3 2 2 1)
        showRotating' axis scale angle solid t = solid
            & W.uScale scale 
            & W.rotate axis (angle * t)
            & toDiagram
        showRotating = showRotating' (unit _z)

    let codeBlockWithFade blockname nextScene = do
            fullCode <- CodeScene.codeScene def 1.6
                =<< CodeBlock.loadCodeBlock "spatula/Object.hs" blockname
            addSvgDuration 1.2 fullCode

            fadedCode <- animate 0.4 
                ( SvgUtils.addBackground SvgUtils.white 
                . SvgUtils.makeOpaque fullCode 
                . (+ 1.0)
                . (* (-0.925))
                . easeInOutSin
                )

            nextScene fadedCode

    let codeBlockWithDiagram blockname diagram = 
            codeBlockWithFade blockname $ \background -> 
                WaterfallScene.stillClipWithBackground def 2 background diagram

    let codeBlockWithObject blockname object = do
            codeBlockWithFade blockname $ \background -> 
                WaterfallScene.animatedClipWithBackground def 5 background $ 
                    showRotating (1/150) (2*pi) (center object)  
                    . easeInOutStay

    let audioCodeBlockParams = 
            def
            & CodeScene.codeSceneStyle .~ Sky.breezeDark
            
    let codeBlockAudioOno = codeBlockOno' "spatula/Audio.hs" audioCodeBlockParams

    let codeBlockTidalAudio blockname tidalEffect = do
            still <- CodeScene.codeScene audioCodeBlockParams 2.0
                =<< CodeBlock.loadCodeBlock "spatula/Audio.hs" blockname
            _ <- tidalEffect
            addSvgDuration 1.0 still

    codeBlockOno "Intro"

    codeBlockAudioOno "Intro"
    setTidalOp Tidal.resetCycles
    setTidalPattern Audio.intro
    codeBlockAudioOno "Audio Intro"
    codeBlockAudioOno "Play Intro"

    codeBlockOno "Imports"
    codeBlockWithDiagram "Profile"
        (Object.bladeProfile 
            & W.pathDiagram W.OutLine W.Visible
            & W.uScale2D (1/80)
            & WaterfallScene.centerDiagram
        )
    setTidalPattern Audio.verse1
    codeBlockAudioOno "Verse 1"
    codeBlockAudioOno "Play Verse 1"
        
    codeBlockWithObject "Sharp Blade" Object.sharpBlade

    codeBlockWithFade "Blade" $ \background -> do
        WaterfallScene.animatedClipWithBackground def 2 background $ 
                toDiagram
                . W.uScale (1/150)
                . center
                . Object.animatedBlade
                . easeInOutStay
        WaterfallScene.animatedClipWithBackground def 5 background $ 
            showRotating (1/150) (2*pi) (center Object.blade)  
                . easeInOutStay

    setTidalPattern Audio.verse2
    setTidalOp $ Tidal.jumpMod 1 8 Audio.verse2'
    codeBlockAudioOno "Verse 2"
    codeBlockAudioOno "Tempo"
    codeBlockAudioOno "Play Verse 2" 

    codeBlockWithDiagram "Slot Profile"
        (Object.slotProfile 
            & W.pathDiagram W.OutLine W.Visible
            & W.uScale2D (1/80)
            & WaterfallScene.centerDiagram
        )
    let redScene = def 
            & WaterfallScene.waterfallVisibleColor .~ JP.PixelRGBA8 155 0 0 255 
            & WaterfallScene.waterfallHiddenColor .~ JP.PixelRGBA8 255 200 200 255

    codeBlockWithFade "Slotted Blade" $ \background -> do
        WaterfallScene.animatedClipWithBackground redScene 0.8 background $ 
                toDiagram
                . W.uScale (1/150)
                . centerBasedOn Object.blade
                . Object.growSlot
                . easeInOutSin
        WaterfallScene.animatedClipWithBackground redScene 0.8 background $ 
                toDiagram
                . W.uScale (1/150)
                . centerBasedOn Object.blade
                . Object.sweepSlots
                . easeInOutSin
        WaterfallScene.animatedClipWithBackground def 5 background $ 
            showRotating (1/150) (2*pi) (center Object.slottedBlade)  
                . easeInOutStay

    codeBlockOno "Handle Params"
    codeBlockOno "Handle Path"
    codeBlockWithFade "Handle" $ \background -> do
        WaterfallScene.animatedClipWithBackground def 2 background $ 
                toDiagram
                . W.uScale (1/150)
                . centerBasedOn Object.handle
                . Object.animatedHandle
                . easeInOutSin
        WaterfallScene.animatedClipWithBackground def 5 background $ 
            showRotating (1/150) (2*pi) (center Object.handle)  
                . easeInOutStay

    -- setTidalOp $ Tidal.jumpMod 1 8 Audio.verse1

    codeBlockWithFade "Handle And Grip" $ \background -> do 
        WaterfallScene.animatedClipWithBackground def 1 background $ 
                toDiagram
                . W.uScale (1/150)
                . centerBasedOn Object.handleWithGrip
                . Object.growGrip 
                . easeInOutSin
        
        WaterfallScene.animatedClipWithBackground def 1 background $ 
                toDiagram
                . W.uScale (1/150)
                . centerBasedOn Object.handleWithGrip
                . Object.roundOffGrip 
                . easeInOutSin
        
        WaterfallScene.animatedClipWithBackground def 5 background $ 
            showRotating (1/150) (2*pi) (center Object.handleWithGrip)  
                . easeInOutStay

    -- doVerse2

    codeBlockWithFade "Hole" $ \background -> do

        WaterfallScene.animatedClipWithBackground redScene 0.75 background $ 
                toDiagram
                . W.uScale (1/150)
                . centerBasedOn Object.handleWithGrip
                . Object.growHole
                . easeInOutSin
        
        WaterfallScene.animatedClipWithBackground redScene 0.75 background $ 
                toDiagram
                . W.uScale (1/150)
                . centerBasedOn Object.handleWithGrip
                . Object.roundOffHole
                . easeInOutSin

        WaterfallScene.animatedClipWithBackground def 0.75 background $ 
                toDiagram
                . W.uScale (1/150)
                . centerBasedOn Object.handleWithGrip
                . Object.growHoleInHandle 
                . easeInOutSin
        
        WaterfallScene.animatedClipWithBackground def 0.75 background $ 
                toDiagram
                . W.uScale (1/150)
                . centerBasedOn Object.handleWithGrip
                . Object.roundOffHoleInHandle
                . easeInOutSin
        
        WaterfallScene.animatedClipWithBackground def 5 background $ 
            showRotating (1/150) (2*pi) (center Object.handleWithHole)  
                . easeInOutStay
        
        
    codeBlockOno "Negative Mask" 
    codeBlockOno "Spatula"

    WaterfallScene.animatedClip def 8 $ showRotating (1/150) (2*pi)
            ( Object.spatula 
                & center
            ) . easeInOutStay
            
    WaterfallScene.animatedClip def 8 $ showRotating' (unit _y) (1/150) (2*pi)
            ( Object.spatula 
                & center
            ) . easeInOutStay

    WaterfallScene.animatedClip def 8 $ showRotating' (Object.handleLongLeg) (1/150) (2*pi)
            ( Object.spatula 
                & center
            ) . easeInOutStay

    setTidalOp $ Tidal.jumpMod 1 8 silence

    WaterfallScene.animatedClip def 8 $ showRotating' (unit _x) (1/150) (2*pi)
            ( Object.spatula 
                & center
            ) . easeInOutStay

    codeBlockOno "Outro" 
    codeBlockAudioOno "Outro"
--}