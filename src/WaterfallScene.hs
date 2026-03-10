module WaterfallScene 
( solidClip
, stillClip
, animatedClip
, centerDiagram
)
where

import VideoProps
import SvgUtils
import GenerateVideo (BuildVideo, WriteFrames, addSvgFrame, addSvgDuration)

import qualified Waterfall as W
import qualified Waterfall.SVG as W
import qualified Graphics.Svg as Svg
import qualified Codec.Picture.Types as JP
import Linear
import Control.Lens
import Data.Foldable (traverse_)
import Effectful
import Effectful.Reader.Static

nFrames :: Integer 
nFrames = 400

border :: Integer 
border = 20

resizeDiagram :: VideoProps -> W.Diagram -> (V2 Double, W.Diagram)
resizeDiagram vd d = 
    case W.diagramBoundingBox d of 
        Nothing -> (zero, d)
        Just (lo, hi) -> 
            let diagramWidth = (hi-lo) ^. _x
                diagramHeight = (hi-lo) ^. _y
                diagramAspect = diagramWidth / diagramHeight
                targetWidth = fromInteger (vd ^. videoWidth - 2 * border) 
                targetHeight = fromInteger (vd ^. videoHeight - 2 * border)
                videoAspect = targetWidth / targetHeight
                (scale, offset) = 
                    if diagramAspect < videoAspect 
                        then ( targetHeight / diagramHeight
                             , V2 
                                (fromInteger border + (targetWidth - diagramWidth * scale) / 2)
                                (fromInteger border)
                             )
                        else ( targetWidth / diagramWidth
                             , V2 
                                (fromInteger border)
                                (fromInteger border + (targetHeight - diagramHeight * scale) / 2)
                             )
            in (offset, W.uScale2D scale d)

frameSvg :: VideoProps -> W.Solid -> Integer-> Svg.Document
frameSvg (vd@VideoProps {..}) solid frame = 
    let angle = 2 * pi * fromInteger frame / fromInteger nFrames
        viewAngle = V3 2 2 1 
        solid' = W.rotate (unit _z) angle solid
        (V2 offsetX offsetY, diagram) = resizeDiagram vd $ W.solidDiagram viewAngle solid'
        waterfallSvg = W.diagramToSvg diagram
        w = Svg.Num . fromIntegral $ vd ^. videoWidth
        h = Svg.Num .fromIntegral $ vd ^. videoHeight
        background = Svg.RectangleTree $ 
            Svg.defaultSvg 
                & Svg.rectUpperLeftCorner .~ (Svg.Px 0, Svg.Px 0)
                & Svg.rectWidth .~ w
                & Svg.rectHeight .~ h
                & colour (JP.PixelRGBA8 255 255 255 255)
        addOffset a = 
            Svg.defaultSvg
                & Svg.groupChildren .~ a
                & translate offsetX offsetY
                & Svg.GroupTree
                & pure
    in waterfallSvg 
            & Svg.width .~ (Just w)
            & Svg.height .~ (Just h)
            & Svg.elements %~ addOffset
            & Svg.elements %~ (background:)

-- | scale the diagram so that the video shows the range [-1, 1] on the smallest axis
diagramSvg :: VideoProps -> W.Diagram -> Svg.Document
diagramSvg vd diagram = 
    let minorAxis = fromIntegral $ min (vd ^. videoWidth) (vd ^. videoHeight)

        w = Svg.Num . fromIntegral $ vd ^. videoWidth
        h = Svg.Num .fromIntegral $ vd ^. videoHeight

        paths lt visibility =
            W.path2DToPathCommands =<<
                W.diagramLines lt visibility (W.uScale2D minorAxis diagram)

        background = Svg.RectangleTree $ 
            Svg.defaultSvg 
                & Svg.rectUpperLeftCorner .~ (Svg.Px 0, Svg.Px 0)
                & Svg.rectWidth .~ w
                & Svg.rectHeight .~ h
                & colour (JP.PixelRGBA8 255 255 255 255)

        dx = (fromInteger (vd ^. videoWidth) ) / 2
        dy = (fromInteger (vd ^. videoHeight) ) / 2
        addOffset = translate dx dy

        document e = Svg.Document Nothing (Just w) (Just h) [e] mempty mempty mempty mempty
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
            in document . group $
                    background :
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
    addSvgDuration duration (diagramSvg vd d)

-- | parameterized by a value that ranges between [0, 1]
animatedClip :: 
    ( BuildVideo :> es
    , WriteFrames :> es
    , Reader VideoProps :> es
    ) => Double -> (Double -> W.Diagram)  -> Eff es ()
animatedClip duration f = do
    vd <- ask
    let nFrames = floor (duration * fromInteger (vd ^. videoFPS))

    let v = (/ fromInteger nFrames) . fromInteger
    traverse_ (addSvgFrame . diagramSvg vd . f . v) [0..nFrames]


renderFrameSvg :: (BuildVideo :> es, WriteFrames :> es, Reader VideoProps :> es) => W.Solid -> Integer -> Eff es ()
renderFrameSvg s i = do
    vd <- ask
    addSvgFrame (frameSvg vd s i)
    
solidClip :: (BuildVideo :> es, WriteFrames :> es, Reader VideoProps :> es) => W.Solid -> Eff es ()
solidClip solid = traverse_ (renderFrameSvg solid) [0..nFrames]
