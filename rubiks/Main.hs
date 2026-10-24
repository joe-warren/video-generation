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
import GenerateAudio (setTidalPattern, setTidalOp, setTidalOpEarly)
import GenerateVideo (addSvgDuration)
import CodeScene (codeScene, codeSceneBackground)
import Control.Monad (join)
import WaterfallScene (WaterfallSceneProps(WaterfallSceneProps))
import CircularWipeColour (circularWipeColour)
import Transitions (easeInOutSin)

videoProps :: VideoProps
videoProps = def
    & videoOutputFile .~ "rubik.mp4"
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

codeBlockOno' tf duration srcfile props blockname =
    let fac = if duration > 1.2 then 1.2 else duration * 0.8
    in addSvgDuration (duration * tf - fac * tf)
            =<< CodeScene.codeScene props (fac * tf)
            =<< CodeBlock.loadCodeBlock srcfile blockname

-- | tf = one Bar
tfIntro = (4*60/Audio.bpmIntro)

tfVerse = (4*60/Audio.bpmVerse)

objectFile = "rubiks/Object.hs"
codeBlockOno tf = codeBlockOno' tf 2 objectFile def

audioFile = "rubiks/Audio.hs"
audioCodeBlockParams :: CodeScene.CodeSceneProps
audioCodeBlockParams = 
    def
    & CodeScene.codeSceneStyle .~ Sky.breezeDark

codeBlockAudioOno tf = codeBlockOno' tf 2 audioFile audioCodeBlockParams


codeBlockAudioOnoFast tf = codeBlockOno' tf 1 audioFile audioCodeBlockParams
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
    & diagram
showRotating = showRotating' (unit _z)

codeBlockWithFade tf blockname nextScene = do
    fullCode <- CodeScene.codeScene def (1.0 * tf)
        =<< CodeBlock.loadCodeBlock objectFile blockname

    addSvgDuration (0.6 * tf) fullCode

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

diffBlockWithFade tf blockNameBefore blockName nextScene = do
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

codeBlockWithDiagram tf blockname diagram = 
    codeBlockWithFade tf blockname $ \background -> 
        WaterfallScene.stillClipWithBackground waterfallProps (3 * tf) background diagram

codeBlockWithObject tf blockname object = do
    codeBlockWithFade tf blockname $ \background -> 
        WaterfallScene.animatedClipWithBackground waterfallProps (3*tf) background $ 
            showRotating (1/7) (2*pi) (center object)  
            . easeInOutStay

diagram :: W.Solid -> W.Diagram        
diagram = W.solidDiagram (V3 2 2 1) . W.uScale (0.02)

main :: IO ()
main = Run.run videoProps $ do 
    codeBlockOno tfIntro "Imports"

    setTidalOp Tidal.resetCycles


    setTidalPattern Audio.intro
    codeBlockAudioOnoFast tfIntro "Melody"
    codeBlockAudioOnoFast tfIntro "Vibes"

    -- 2

    codeBlockWithFade tfIntro "Single Segment" $ \background -> do
        WaterfallScene.animatedClipWithBackground def (1*tfIntro) background $ 
            diagram
            . Object.singleSegmentStage1 
            . Transitions.easeInOutSin
        WaterfallScene.animatedClipWithBackground def (1*tfIntro) background $ 
            diagram
            . Object.singleSegmentStage2
            . Transitions.easeInOutSin
    
    -- 6

    codeBlockWithFade tfIntro "Combined Segments" $ \background -> do
        WaterfallScene.animatedClipWithBackground waterfallProps (4*tfIntro) background $ 
            diagram
            . Object.combinedSegmentsAnimated 
            . Transitions.easeInOutSin

    -- 12

    setTidalOpEarly 0.1 $ Tidal.jumpMod 1 2 Audio.introPercussion
    codeBlockAudioOno tfIntro "Intro With Percussion"
    -- 14

    codeBlockWithFade tfIntro "Single Sticker" $ \background -> do
        WaterfallScene.animatedClipWithBackground def (tfIntro) background $ 
            diagram
            . Object.singleStickerStage1 
            . Transitions.easeInOutSin
        WaterfallScene.animatedClipWithBackground def (tfIntro) background $ 
            diagram
            . Object.singleStickerStage2
            . Transitions.easeInOutSin
    -- 18

    codeBlockWithFade tfIntro "One Side Of Stickers" $ \background -> do
        WaterfallScene.animatedClipWithBackground waterfallProps (2*tfIntro) background $ 
            diagram
            . Object.oneSideOfStickersAnimated
            . Transitions.easeInOutSin
    -- 22

    codeBlockWithFade tfIntro "Segments With Stickers" $ \background -> do
        WaterfallScene.animatedClipWithBackground waterfallProps (4*tfIntro) background $ 
            diagram
            . Object.segmentsWithStickersAnimated
            . Transitions.easeInOutSin

        WaterfallScene.animatedClipWithBackground waterfallProps (4*tfIntro) background $
            diagram . Object.scramble Object.segmentsWithStickers
    --32

    setTidalOpEarly 0.1 $ Tidal.jumpMod 1 4 Audio.verse
    codeBlockAudioOnoFast (tfVerse/2) "Verse Parts"
    codeBlockAudioOnoFast (tfVerse/2) "Keys"
    codeBlockAudioOnoFast (tfVerse/2) "Bass"
    codeBlockAudioOnoFast (tfVerse/2) "Lead"
    codeBlockAudioOnoFast (tfVerse/2) "FM Drum Instrument"
    codeBlockAudioOnoFast (tfVerse/2) "FM Drum Stacked"
    codeBlockAudioOnoFast tfVerse "Play Verse"

    --36

    diffBlockWithFade tfVerse "Segments With Stickers" "Segments With Stickers With Colour" $ \background -> do
        -- 38
        circularWipeColour tfVerse
            . WaterfallScene.diagramSvg 
                waterfallProps
                background
                videoProps 
                . diagram $ 
                    Object.scramble
                    Object.segmentsWithStickersWithColour
                    1
        --39

        WaterfallScene.animatedClipWithBackground waterfallProps (3*tfVerse) background $ 
            showRotating 1 (2*pi) (Object.scramble Object.segmentsWithStickersWithColour 1)
            . Transitions.easeInOutBack

        -- 42

        WaterfallScene.animatedClipWithBackground waterfallProps (4*tfVerse) background $
            diagram 
            . Object.scramble Object.segmentsWithStickersWithColour 
            . (1 -)
            . easeInOutSin

        -- 46
    
        WaterfallScene.animatedClipWithBackground waterfallProps (4*tfVerse) background $ 
            showRotating 1 (2*pi) Object.segmentsWithStickersWithColour
            . Transitions.easeInOutBack

        -- 50

        setTidalOpEarly 0.1 $ Tidal.jumpMod 1 4 silence
    
        WaterfallScene.stillClipWithBackground waterfallProps (tfVerse * 6) background $ 
            diagram Object.segmentsWithStickersWithColour
        -- 54
        
    {--
    let background = SvgUtils.blankCanvas videoProps SvgUtils.white
    WaterfallScene.animatedClipWithBackground waterfallProps (4*tfVerse) background $
        diagram 
        . Object.scramble Object.segmentsWithStickersWithColour 
        . (1 -)
        . easeInOutSin
    --}
            
    