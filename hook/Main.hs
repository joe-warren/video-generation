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
import ImageScene (addImageDuration, loadImage)
import CodeScene (codeScene, codeSceneBackground)
import Control.Monad (join)
import qualified Sound.Tidal.Boot as Audio

videoProps :: VideoProps
videoProps = def
    & videoOutputFile .~ "hook.mp4"
    & videoGenerateAudio .~ True
    & videoGenerateVideo .~ True
    & videoConvertViaPng .~ False

easeInOutStay :: Double -> Double
easeInOutStay x 
    | x < fac = Transitions.easeInOutBack (x / fac)
    | otherwise = 1
    where fac = 0.9

animateDiagramLines :: W.Diagram -> Double -> W.Diagram
animateDiagramLines diagram fraction = 
    mconcat 
        [ W.pathDiagram lineType visibility (W.takePathFraction fraction line)
        | lineType <- [W.OutLine, W.SharpLine]
        , visibility <- [W.Visible, W.Hidden]
        , line <-  W.diagramLines lineType visibility diagram
        ]

codeBlockOno' duration srcfile props blockname = 
            addSvgDuration duration
            =<< CodeScene.codeScene props (1.2 * tf)
            =<< CodeBlock.loadCodeBlock srcfile blockname

-- 8 beats to a bar, 3 beats per second, ( tf = half a bar)
tf = 8/6

objectFile = "hook/Object.hs"
codeBlockOno = codeBlockOno' (0.8 * tf) objectFile def

audioFile = "hook/Audio.hs"
audioCodeBlockParams :: CodeScene.CodeSceneProps
audioCodeBlockParams = 
    def
    & CodeScene.codeSceneStyle .~ Sky.breezeDark

codeBlockAudioOno = codeBlockOno' (0.8 * tf) audioFile audioCodeBlockParams

centerBasedOn target solid = 
    case W.axisAlignedBoundingBox target of
        Just (lo, hi) -> W.translate ((lo + hi) ^* (-0.5)) solid
        Nothing -> solid

center = join centerBasedOn

toDiagram = W.solidDiagram (V3 2 2 1)
showRotating' axis scale angle solid t =
    solid
    & W.uScale scale 
    & W.rotate axis (angle * t)
    & toDiagram
showRotating = showRotating' (unit _z)

codeBlockWithFade blockname nextScene = do
    fullCode <- CodeScene.codeScene def (1.6 * tf)
        =<< CodeBlock.loadCodeBlock objectFile blockname
    addSvgDuration (1.0 * tf) fullCode

    fadedCode <- animate (0.4  * tf)
        ( SvgUtils.addBackground SvgUtils.white 
        . SvgUtils.makeOpaque fullCode 
        . (+ 1.0)
        . (* (-0.925))
        . easeInOutSin
        )

    nextScene fadedCode

diffBlockWithFade blockNameBefore blockName nextScene = do
    codeBefore <- CodeBlock.loadCodeBlock objectFile blockNameBefore
    codeAfter <- CodeBlock.loadCodeBlock objectFile blockName

    fullCode <- CodeScene.diffScene def (1.6 * tf) codeBefore codeAfter

    addSvgDuration (1.0*tf) fullCode

    fadedCode <- animate (0.4 * tf)
        ( SvgUtils.addBackground SvgUtils.white 
        . SvgUtils.makeOpaque fullCode 
        . (+ 1.0)
        . (* (-0.925))
        . easeInOutSin
        )

    nextScene fadedCode

codeBlockWithDiagram blockname diagram = 
    codeBlockWithFade blockname $ \background -> 
        WaterfallScene.stillClipWithBackground def (3 * tf) background diagram

codeBlockWithObject blockname object = do
    codeBlockWithFade blockname $ \background -> 
        WaterfallScene.animatedClipWithBackground def (3*tf) background $ 
            showRotating (1/7) (2*pi) (center object)  
            . easeInOutStay

propertiesDiffBlock blocknameA blocknameB propertiesBefore propertiesAfter = 
    diffBlockWithFade blocknameA blocknameB $ \background -> do
        WaterfallScene.animatedClipWithBackground def (3*tf) background $ 
            toDiagram
                . W.uScale (1/7)
                . W.translate (negate $ unit _z) 
                . (Object.interpolatedHook propertiesBefore propertiesAfter) 
                . easeInOutSin
        WaterfallScene.animatedClipWithBackground def (4*tf) background $ 
            showRotating (1/7) (2*pi) (W.translate (negate $ unit _z) $ Object.hook propertiesAfter)  
                . easeInOutStay

codeBlockWithImageBackground blockname imageFilename alignment duration = do 
    image <- loadImage imageFilename
    let props = def 
            & CodeScene.codeSceneAlignment .~ alignment
            & CodeScene.codeSceneBackground .~ const (const image) 
    codeBlockOno' duration objectFile props blockname

main :: IO ()
main = Run.run videoProps $ do
    codeBlockWithImageBackground "Intro" "hook/images/thingiverse.png" CodeScene.Bottom (2.8 * tf)

    codeBlockOno "Imports"

    setTidalOp Tidal.resetCycles
    setTidalPattern Audio.intro
    codeBlockAudioOno "Audio Intro"
    codeBlockAudioOno "Pipes"
    codeBlockAudioOno "Vibes"
    codeBlockAudioOno "Play Intro"

    setTidalOp $ Tidal.jumpMod 1 4 (Audio.verse1)
    --setTidalPattern Audio.verse1

    codeBlockAudioOno "Verse 1"
    codeBlockAudioOno "Play Verse 1"

    codeBlockOno "Properties Def"

    codeBlockOno "Degrees"

    codeBlockOno "Properties Value"

    codeBlockOno "Hoop and Shaft"
    codeBlockOno "Hook Curve"

    codeBlockWithFade "Single Hook Wire" $ \background -> do
        WaterfallScene.animatedClipWithBackground def (3*tf) background $ 
            toDiagram
                . W.uScale (1/7)
                . W.translate (negate $ unit _z) 
                . (Object.singleHookWireAnimation Object.properties) 
                . easeInOutSin
        WaterfallScene.animatedClipWithBackground def (4*tf) background $ 
            showRotating (1/7) (2*pi) (W.translate (negate $ unit _z) $ Object.singleHookWire Object.properties)  
                . easeInOutStay

    setTidalOp $ Tidal.jumpMod 1 4 (Audio.verse2)
    codeBlockAudioOno "Verse 2"
    codeBlockAudioOno "Play Verse 2"

    codeBlockOno "Half Arrowhead"

    codeBlockWithFade "Arrowhead" $ \background -> do
        let position = W.uScale (1/3) . W.translate (negate $ unit _z ^* 0.5)
        WaterfallScene.animatedClipWithBackground def (1.5*tf) background $ 
            animateDiagramLines 
                (toDiagram . position $ Object.defaultHalfArrowhead)
            . easeInOutStay 
        WaterfallScene.animatedClipWithBackground def (1.5*tf) background $ 
            toDiagram
                . position
                . (Object.defaultArrowheadAnimation) 
                . easeInOutSin
        WaterfallScene.animatedClipWithBackground def (4*tf) background $ 
            showRotating (1) (2*pi) 
                    (position $ Object.defaultArrowheadAnimation 1)  
                . easeInOutStay

    diffBlockWithFade "Single Hook Wire" "Single Hook With Arrowhead" $ \background -> do
        WaterfallScene.animatedClipWithBackground def (3*tf) background $ 
            toDiagram
                . W.uScale (1/7)
                . W.translate (negate $ unit _z) 
                . (Object.singleHookWithArrowheadAnimation Object.properties) 
                . easeInOutSin
        WaterfallScene.animatedClipWithBackground def (4*tf) background $ 
            showRotating (1/7) (2*pi) (W.translate (negate $ unit _z) $ Object.singleHookWithArrowhead Object.properties)  
                . easeInOutStay

    setTidalOp $ Tidal.jumpMod 1 4 (Audio.verse2')

    diffBlockWithFade "Single Hook With Arrowhead" "Whole Hook" $ \background -> do
        WaterfallScene.animatedClipWithBackground def (3*tf) background $ 
            toDiagram
                . W.uScale (1/7)
                . W.translate (negate $ unit _z) 
                . (Object.animateInHook Object.properties) 
                . easeInOutSin
        WaterfallScene.animatedClipWithBackground def (4*tf) background $ 
            showRotating (1/7) (2*pi) (W.translate (negate $ unit _z) $ Object.hook Object.properties)  
                . easeInOutStay

    setTidalOp $ Tidal.jumpMod 1 4 (Audio.verse2Alt)

    propertiesDiffBlock "Properties Value" "Properties Rotated Head" Object.properties Object.propertiesRotatedHead

    propertiesDiffBlock "Properties Rotated Head" "Properties Five Hooks" Object.propertiesRotatedHead Object.propertiesFiveHooks
    
    propertiesDiffBlock "Properties Five Hooks" "Properties High Sweep Angle" Object.propertiesFiveHooks Object.propertiesHighSweepAngle
 
    propertiesDiffBlock "Properties High Sweep Angle" "Properties Low Sweep Angle" Object.propertiesHighSweepAngle Object.propertiesLowSweepAngle

    propertiesDiffBlock "Properties Low Sweep Angle" "Properties Chonky" Object.propertiesLowSweepAngle Object.propertiesChonky

    setTidalOp $ Tidal.jumpMod 1 4 (Audio.playPlayout)

    codeBlockWithImageBackground "Printed" "hook/images/printed.jpg" CodeScene.Bottom (6.8*tf)
    
    setTidalOp $ Tidal.jumpMod 1 4 (Audio.silence)
