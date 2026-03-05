module SvgUtils 
( translate
, colour
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

translate :: Svg.WithDrawAttributes a => Double -> Double -> a -> a
translate x y elem = 
    let t = Svg.Translate x y
        addT = Just . maybe [t] (t:)
    in elem & Svg.drawAttr . Svg.transform %~ addT

colour :: Svg.WithDrawAttributes a => JP.PixelRGBA8 -> a -> a    
colour c a = a & Svg.drawAttr . Svg.fillColor .~ (Last . Just $ Svg.ColorRef c)