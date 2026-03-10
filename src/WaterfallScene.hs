module WaterfallScene 
( stillClip
, stillClipWithBackground
, animatedClip
, animatedClipWithBackground
, centerDiagram
)
where

import VideoProps
import SvgUtils
import GenerateVideo (BuildVideo, WriteFrames, addSvgDuration)

import qualified Waterfall as W
import qualified Waterfall.SVG as W
import qualified Graphics.Svg as Svg
import qualified Codec.Picture.Types as JP
import Linear
import Control.Lens
import Effectful
import Effectful.Reader.Static
import Animate (animate)
import Control.Monad (void)

-- | scale the diagram so that the video shows the range [-1, 1] on the smallest axis
diagramSvg :: Svg.Document -> VideoProps -> W.Diagram -> Svg.Document
diagramSvg background vd diagram = 
    let minorAxis = fromIntegral $ min (vd ^. videoWidth) (vd ^. videoHeight)

        w = Svg.Num . fromIntegral $ vd ^. videoWidth
        h = Svg.Num .fromIntegral $ vd ^. videoHeight

        paths lt visibility =
            W.path2DToPathCommands =<<
                W.diagramLines lt visibility (W.uScale2D minorAxis diagram)

        dx = (fromInteger (vd ^. videoWidth) ) / 2
        dy = (fromInteger (vd ^. videoHeight) ) / 2
        addOffset = translate dx dy

        document e = 
            background 
                & Svg.elements %~ (<> e)
            
        pathColour W.Visible = JP.PixelRGBA8 0 0 0 255
        pathColour W.Hidden = JP.PixelRGBA8 200 200 255 255
        pathOf lt visibility = 
            Svg.defaultSvg 
                & Svg.pathDefinition .~ (paths lt visibility)
                & Svg.drawAttr . Svg.fillColor .~ (pure Svg.FillNone)
                & strokeColour (pathColour visibility)
                & strokeWidth 2.0
                & Svg.PathTree 
                & addOffset
            
        group children = Svg.GroupTree $ Svg.Group mempty children Nothing Svg.defaultSvg
            in document . pure . group $
                    [ pathOf lineType visibility
                        | visibility <- [W.Hidden, W.Visible]
                        , lineType <- [W.SharpLine, W.OutLine]
                    ]

centerDiagram :: W.Diagram -> W.Diagram
centerDiagram d = 
    case W.diagramBoundingBox d of 
        Nothing -> d
        Just (lo, hi) -> W.translate2D (negate (lo + hi) ^* 0.5) d

stillClip :: 
    ( BuildVideo :> es
    , WriteFrames :> es
    , Reader VideoProps :> es
    ) => Double -> W.Diagram  -> Eff es ()
stillClip duration d = do
    vd <- ask 
    let background =SvgUtils.blankCanvas vd SvgUtils.white
    addSvgDuration duration (diagramSvg background vd d)


stillClipWithBackground :: 
    ( BuildVideo :> es
    , WriteFrames :> es
    , Reader VideoProps :> es
    ) => Double -> Svg.Document -> W.Diagram -> Eff es ()
stillClipWithBackground duration background d = do
    vd <- ask
    addSvgDuration duration (diagramSvg background vd d)

-- | parameterized by a value that ranges between [0, 1]
animatedClip :: 
    ( BuildVideo :> es
    , WriteFrames :> es
    , Reader VideoProps :> es
    ) => Double -> (Double -> W.Diagram)  -> Eff es ()
animatedClip duration f = do
    vd <- ask
    let background = SvgUtils.blankCanvas vd SvgUtils.white
    void $ animate duration (diagramSvg background vd . f )


animatedClipWithBackground :: 
    ( BuildVideo :> es
    , WriteFrames :> es
    , Reader VideoProps :> es
    ) => Double -> Svg.Document -> (Double -> W.Diagram)  -> Eff es ()
animatedClipWithBackground duration background f = do
    vd <- ask
    void $ animate duration (diagramSvg background vd . f )
