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

addWipe :: VideoProps -> Svg.Document -> Double -> Svg.Document
addWipe vd doc frac = 
    let w = realToFrac $ vd ^. videoWidth
        h = realToFrac $ vd ^. videoHeight
        rMax = sqrt (w * w + h * h) / 2
        r = rMax * frac

        rect = Svg.defaultSvg
            & Svg.rectWidth .~ (Svg.Num w) 
            & Svg.rectHeight .~ (Svg.Num h) 
            & Svg.rectUpperLeftCorner .~ (Svg.Num 0, Svg.Num 0)
            & Svg.drawAttr . Svg.fillColor .~ (pure . Svg.ColorRef $ SvgUtils.black)
            & Svg.RectangleTree

        circle = Svg.defaultSvg
            & Svg.circleCenter .~ (Svg.Num (w/2), Svg.Num (h/2))
            & Svg.circleRadius .~ (Svg.Num r)
            & Svg.drawAttr . Svg.fillColor .~ (pure . Svg.ColorRef $  SvgUtils.white)
            & Svg.CircleTree

        mask = Svg.defaultSvg
            & Svg.maskContent .~ [rect, circle]
            & Svg.maskUnits .~ Svg.CoordUserSpace
            & Svg.maskContentUnits .~ Svg.CoordUserSpace
            & Svg.maskPosition .~ (Svg.Num 0, Svg.Num 0)
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
    void $ animate duration (addWipe vd doc)

