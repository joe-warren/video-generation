module WaterfallScene 
( stillClip
, stillClipWithBackground
, animatedClip
, animatedClipWithBackground
, centerDiagram
, WaterfallSceneProps (..)
, waterfallHiddenColor
, waterfallVisibleColor
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

data WaterfallSceneProps = WaterfallSceneProps
    { _waterfallVisibleColor :: JP.PixelRGBA8 
    , _waterfallHiddenColor :: JP.PixelRGBA8
    }

makeLenses ''WaterfallSceneProps

instance Default WaterfallSceneProps where
    def = WaterfallSceneProps 
        { _waterfallVisibleColor = black
        , _waterfallHiddenColor = JP.PixelRGBA8 200 200 255 255
        }

-- | scale the diagram so that the video shows the range [-1, 1] on the smallest axis
diagramSvg :: WaterfallSceneProps -> Svg.Document -> VideoProps -> W.Diagram -> Svg.Document
diagramSvg props background vd diagram = 
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
            
        pathColour W.Visible = props ^. waterfallVisibleColor
        pathColour W.Hidden = props ^. waterfallHiddenColor
        
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
