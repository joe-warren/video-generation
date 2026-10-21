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
import qualified Waterfall.SVG as W

import qualified Skylighting as Sky

import Data.Default (def)
import Control.Lens
import Linear
import Animate (animate)
import qualified SvgUtils
import qualified Sound.Tidal.Safe.Boot as Tidal
import Sound.Tidal.Boot (silence)
import GenerateAudio (setTidalPattern, setTidalOp)
import GenerateVideo (addSvgDuration)
import ImageScene (addImageDuration, loadImage)
import CodeScene (codeScene, codeSceneBackground)
import Control.Monad (join)
import qualified Sound.Tidal.Boot as Audio
import WaterfallScene (WaterfallSceneProps(WaterfallSceneProps))

videoProps :: VideoProps
videoProps = def
    & videoOutputFile .~ "rubik.mp4"
    & videoGenerateAudio .~ False
    & videoGenerateVideo .~ True
    & videoConvertViaPng .~ True

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

objectFile = "rubiks/Object.hs"
codeBlockOno = codeBlockOno' (0.8 * tf) objectFile def

audioFile = "rubiks/Audio.hs"
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
        . Transitions.easeInOutSin
        )

    nextScene fadedCode

waterfallProps = WaterfallSceneProps $
          W.diagramWithVisibleLine 
            ( W.lineWithColour W.black
              <> W.lineWithWidth 3
            ) 
          <> W.diagramWithHiddenLine 
            ( W.lineWithColour (W.black)
              <> W.lineWithOpacity 0.2
              <> W.lineWithWidth 2
            )

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
        . Transitions.easeInOutSin
        )

    nextScene fadedCode

codeBlockWithDiagram blockname diagram = 
    codeBlockWithFade blockname $ \background -> 
        WaterfallScene.stillClipWithBackground waterfallProps (3 * tf) background diagram

codeBlockWithObject blockname object = do
    codeBlockWithFade blockname $ \background -> 
        WaterfallScene.animatedClipWithBackground waterfallProps (3*tf) background $ 
            showRotating (1/7) (2*pi) (center object)  
            . easeInOutStay

diagram :: W.Solid -> W.Diagram        
diagram = W.solidDiagram (V3 2 2 1) . W.uScale (0.02)

main :: IO ()
main = Run.run videoProps $ do 
    {--
    codeBlockOno "Imports"

    setTidalOp Tidal.resetCycles

    setTidalPattern Audio.intro
    codeBlockAudioOno "Audio Intro"

    codeBlockWithFade "Single Segment" $ \background -> do
        WaterfallScene.animatedClipWithBackground def (2*tf) background $ 
            diagram
            . Object.singleSegmentStage1 
            . Transitions.easeInOutSin
        WaterfallScene.animatedClipWithBackground def (2*tf) background $ 
            diagram
            . Object.singleSegmentStage2
            . Transitions.easeInOutSin

    codeBlockWithFade "Combined Segments" $ \background -> do
        WaterfallScene.animatedClipWithBackground waterfallProps (4*tf) background $ 
            diagram
            . Object.combinedSegmentsAnimated 
            . Transitions.easeInOutSin

    codeBlockWithFade "Single Sticker" $ \background -> do
        WaterfallScene.animatedClipWithBackground def (2*tf) background $ 
            diagram
            . Object.singleStickerStage1 
            . Transitions.easeInOutSin
        WaterfallScene.animatedClipWithBackground def (2*tf) background $ 
            diagram
            . Object.singleStickerStage2
            . Transitions.easeInOutSin
    
    codeBlockWithFade "One Side Of Stickers" $ \background -> do
        WaterfallScene.animatedClipWithBackground waterfallProps (4*tf) background $ 
            diagram
            . Object.oneSideOfStickersAnimated
            . Transitions.easeInOutSin

    codeBlockWithFade "Segments With Stickers" $ \background -> do
        WaterfallScene.animatedClipWithBackground waterfallProps (4*tf) background $ 
            diagram
            . Object.segmentsWithStickersAnimated
            . Transitions.easeInOutSin

    --}

    diffBlockWithFade "Segments With Stickers" "Segments With Stickers With Colour" $ \background -> do
        WaterfallScene.stillClipWithBackground waterfallProps (4*tf) background $ diagram Object.segmentsWithStickersWithColour 



