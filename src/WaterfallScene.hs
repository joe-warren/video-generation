module WaterfallScene 
( solidClip
)
where

import VideoData
import SvgUtils

import qualified Waterfall as W
import qualified Waterfall.SVG as W
import qualified Graphics.Svg as Svg
import qualified Codec.Picture.Types as JP
import Linear
import Control.Lens

nFrames :: Integer 
nFrames = 400

border :: Integer 
border = 20

resizeDiagram :: VideoData -> W.Diagram -> (V2 Double, W.Diagram)
resizeDiagram (VideoData {..}) d = 
    case W.diagramBoundingBox d of 
        Nothing -> (zero, d)
        Just (lo, hi) -> 
            let diagramWidth = (hi-lo) ^. _x
                diagramHeight = (hi-lo) ^. _y
                diagramAspect = diagramWidth / diagramHeight
                targetWidth = fromInteger (videoWidth - 2 * border) 
                targetHeight = fromInteger (videoHeight - 2 * border)
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


frameSvg :: VideoData -> W.Solid -> Integer-> Svg.Document
frameSvg (vd@VideoData {..}) solid frame = 
    let angle = 2 * pi * fromInteger frame / fromInteger nFrames
        viewAngle = V3 2 2 1 
        solid' = W.rotate (unit _z) angle solid
        (V2 offsetX offsetY, diagram) = resizeDiagram vd $ W.solidDiagram viewAngle solid'
        waterfallSvg = W.diagramToSvg diagram
        w = Svg.Num . fromIntegral $ videoWidth
        h = Svg.Num .fromIntegral $ videoHeight
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

renderFrameSvg :: VideoData -> W.Solid -> Integer -> IO (Int, FilePath)
renderFrameSvg vd s i =
    let frame = frameSvg vd s i
        path = (scratchDir vd <> "/" <> show i <> ".svg")
    in do 
        Svg.saveXmlFile path frame
        return (1, path)
    

solidClip :: VideoData -> W.Solid -> IO [(Int, FilePath)]
solidClip vd solid = traverse (renderFrameSvg vd solid) [0..nFrames]
