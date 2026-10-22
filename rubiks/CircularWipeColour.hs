module CircularWipeColour 
(circularWipeColour
) where

import qualified Graphics.Svg as Svg
import qualified Graphics.Svg.CssTypes as Svg.Css
import VideoProps
import Effectful
import Effectful.Reader.Static
import Animate (animate)
import GenerateVideo (BuildVideo, addSvgDuration)
import Control.Lens
import Control.Monad (void)
import qualified Data.Map as M
import qualified SvgUtils
import qualified Transitions

addWipe :: VideoProps -> Svg.Document -> Double -> Svg.Document
addWipe vd doc frac = 
    let w = realToFrac $ vd ^. videoWidth
        h = realToFrac $ vd ^. videoHeight
        -- fix me if the coloured part of the image ever isn't confined to the center 
        rMax = h / 2
        r = rMax * frac

        -- userSpaceOnUse is the coordinate system of the masked .fill elements,
        -- which diagramSvg has translated so that the origin is the centre of the frame
        rect = Svg.defaultSvg
            & Svg.rectWidth .~ (Svg.Num w)
            & Svg.rectHeight .~ (Svg.Num h)
            & Svg.rectUpperLeftCorner .~ (Svg.Num (-w/2), Svg.Num (-h/2))
            & Svg.drawAttr . Svg.fillColor .~ (pure . Svg.ColorRef $ SvgUtils.black)
            & Svg.RectangleTree

        circle = Svg.defaultSvg
            & Svg.circleCenter .~ (Svg.Num 0, Svg.Num 0)
            & Svg.circleRadius .~ (Svg.Num r)
            & Svg.drawAttr . Svg.fillColor .~ (pure . Svg.ColorRef $  SvgUtils.white)
            & Svg.CircleTree

        mask = Svg.defaultSvg
            & Svg.maskContent .~ [rect, circle]
            & Svg.maskUnits .~ Svg.CoordUserSpace
            & Svg.maskContentUnits .~ Svg.CoordUserSpace
            & Svg.maskPosition .~ (Svg.Num (-w/2), Svg.Num (-h/2))
            & Svg.maskWidth .~ (Svg.Num w)
            & Svg.maskHeight .~ (Svg.Num h)
            & Svg.ElementMask
            & M.singleton "wipe"

        newStyles = 
            [ Svg.Css.CssRule
            [[Svg.Css.AllOf [Svg.Css.OfClass "fill"]]]
            [ Svg.Css.CssDeclaration "mask" [[Svg.Css.CssFunction "url" . pure $ Svg.Css.CssReference "wipe"]]]
            ]
        in doc
            & Svg.definitions %~ (<> mask)
            & Svg.styleRules %~ (<> newStyles)

circularWipeColour
    :: ( BuildVideo :> es
    , Reader VideoProps :> es
    ) => Double -> Svg.Document -> Eff es ()
circularWipeColour duration doc = do
    vd <- ask 
    void $ animate duration (addWipe vd doc . Transitions.easeInOutSin)

