module SvgUtils 
( translate
, colour
, strokeColour
, strokeWidth
, addBackground
, makeOpaque
, blankCanvas
, white
, black
) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Skylighting as Sky
import qualified Graphics.Svg as Svg
import qualified Codec.Picture.Types as JP
import Control.Lens
import Control.Monad (join, forM)
import Data.Monoid (Last (..))
import Control.Arrow (second)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import GHC.Float (double2Float)
import VideoProps

translate :: Svg.WithDrawAttributes a => Double -> Double -> a -> a
translate x y elem = 
    let t = Svg.Translate x y
        addT = Just . maybe [t] (t:)
    in elem & Svg.drawAttr . Svg.transform %~ addT

colour :: Svg.WithDrawAttributes a => JP.PixelRGBA8 -> a -> a    
colour c a = a & Svg.drawAttr . Svg.fillColor .~ (Last . Just $ Svg.ColorRef c)

strokeColour :: Svg.WithDrawAttributes a => JP.PixelRGBA8 -> a -> a    
strokeColour c a = a & Svg.drawAttr . Svg.strokeColor .~ (Last . Just $ Svg.ColorRef c)

strokeWidth :: Svg.WithDrawAttributes a => Double -> a -> a    
strokeWidth w a = a & Svg.drawAttr . Svg.strokeWidth .~ (Last . Just . Svg.Px $ w)

white :: JP.PixelRGBA8
white = JP.PixelRGBA8 255 255 255 255

black :: JP.PixelRGBA8
black = JP.PixelRGBA8 0 0 0 255

addBackground :: JP.PixelRGBA8 -> Svg.Document -> Svg.Document
addBackground c d = 
    let w = fromMaybe (Svg.Num 0) $ d ^. Svg.width
        h = fromMaybe (Svg.Num 0) $ d ^. Svg.height
        rect =
            Svg.defaultSvg 
                & Svg.rectUpperLeftCorner .~ (Svg.Px 0, Svg.Px 0)
                & Svg.rectWidth .~ w
                & Svg.rectHeight .~ h
                & colour c
                & Svg.RectangleTree
    in d & Svg.elements %~ (rect :)

blankCanvas :: VideoProps -> JP.PixelRGBA8 -> Svg.Document
blankCanvas vd c =
    let w = Svg.Num . fromIntegral $ vd ^. videoWidth
        h = Svg.Num .fromIntegral $ vd ^. videoHeight
    in Svg.Document Nothing (Just w) (Just h) mempty mempty mempty mempty mempty
        & addBackground c

makeOpaque :: Svg.Document -> Double -> Svg.Document
makeOpaque d opacity = 
    let opaqueGroup children = 
            Svg.defaultSvg 
                & Svg.groupChildren .~ children
                & Svg.drawAttr . Svg.groupOpacity .~ Just (double2Float opacity)
                & Svg.GroupTree
    in d & 
        Svg.elements  %~ (pure . opaqueGroup)