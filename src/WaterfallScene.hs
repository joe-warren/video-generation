module WaterfallScene 
( stillClip
, stillClipWithBackground
, animatedClip
, animatedClipWithBackground
, centerDiagram
, diagramSvg
, WaterfallSceneProps (..)
)
where

import VideoProps
import SvgUtils
import GenerateVideo (BuildVideo, addSvgDuration)

import qualified Waterfall as W
import qualified Waterfall.SVG as W
import qualified Graphics.Svg as Svg
import qualified Codec.Picture.Types as JP
import Linear
import Control.Lens
import Effectful
import Effectful.Reader.Static
import Data.Default
import Animate (animate)
import Control.Monad (void)

newtype WaterfallSceneProps = WaterfallSceneProps 
    { propsLineStyle :: W.DiagramStyle
    }

instance Default WaterfallSceneProps where
    def = WaterfallSceneProps $ 
          W.diagramWithVisibleLine 
            ( W.lineWithColour W.black
            ) 
          <> W.diagramWithHiddenLine 
            ( W.lineWithColour (W.rgb24 0xC8C8FF)
              <> W.lineWithOpacity 1
            )

-- | scale the diagram so that the video shows the range [-1, 1] on the smallest axis
diagramSvg :: WaterfallSceneProps -> Svg.Document -> VideoProps -> W.Diagram -> Svg.Document
diagramSvg props background vd diagram = 
    let minorAxis = fromIntegral $ min (vd ^. videoWidth) (vd ^. videoHeight)

        w = Svg.Num . fromIntegral $ vd ^. videoWidth
        h = Svg.Num .fromIntegral $ vd ^. videoHeight

        dx = (fromInteger (vd ^. videoWidth) ) / 2
        dy = (fromInteger (vd ^. videoHeight) ) / 2

        addOffset = translate dx dy

        diagramDoc = W.diagramToSvgWithLineStyle (propsLineStyle props) (W.uScale2D minorAxis diagram)
        diagramElement = 
            Svg.defaultSvg 
                & Svg.groupChildren .~ (diagramDoc ^. Svg.elements)
                & addOffset
                & Svg.GroupTree 

        in background 
            & Svg.elements %~ (<> [diagramElement])
            & Svg.styleRules %~ (<> (diagramDoc ^. Svg.styleRules) )
            
centerDiagram :: W.Diagram -> W.Diagram
centerDiagram d = 
    case W.diagramBoundingBox d of 
        Nothing -> d
        Just (lo, hi) -> W.translate2D (negate (lo + hi) ^* 0.5) d

stillClip :: 
    ( BuildVideo :> es
    , Reader VideoProps :> es
    ) => WaterfallSceneProps -> Double -> W.Diagram  -> Eff es ()
stillClip props duration d = do
    vd <- ask 
    let background =SvgUtils.blankCanvas vd SvgUtils.white
    addSvgDuration duration (diagramSvg props background vd d)

stillClipWithBackground :: 
    ( BuildVideo :> es
    , Reader VideoProps :> es
    ) => WaterfallSceneProps -> Double -> Svg.Document -> W.Diagram -> Eff es ()
stillClipWithBackground props duration background d = do
    vd <- ask
    addSvgDuration duration (diagramSvg props background vd d)

-- | parameterized by a value that ranges between [0, 1]
animatedClip :: 
    ( BuildVideo :> es
    , Reader VideoProps :> es
    ) => WaterfallSceneProps -> Double -> (Double -> W.Diagram) -> Eff es ()
animatedClip props duration f = do
    vd <- ask
    let background = SvgUtils.blankCanvas vd SvgUtils.white
    void $ animate duration (diagramSvg props background vd . f )

animatedClipWithBackground :: 
    ( BuildVideo :> es
    , Reader VideoProps :> es
    ) => WaterfallSceneProps -> Double -> Svg.Document -> (Double -> W.Diagram)  -> Eff es ()
animatedClipWithBackground props duration background f = do
    vd <- ask
    void $ animate duration (diagramSvg props background vd . f )
