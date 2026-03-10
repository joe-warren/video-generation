module Animate 
( animate 
, animateM
) where

import Effectful 
import Effectful.Reader.Static
import qualified Graphics.Svg as Svg
import Control.Lens
import Data.Foldable (traverse_)
import GenerateVideo
import VideoProps
import Control.Monad ((<=<))
animateM ::
    ( BuildVideo :> es
    , WriteFrames :> es
    , Reader VideoProps :> es
    ) => Double -> (Double -> Eff es Svg.Document) -> Eff es Svg.Document
animateM duration f = do
    vd <- ask
    let nFrames = floor (duration * fromInteger (vd ^. videoFPS))

    let v = (/ fromInteger nFrames) . fromInteger
    traverse_ (addSvgFrame <=< f . v) [0..nFrames]
    f 1

animate ::
    ( BuildVideo :> es
    , WriteFrames :> es
    , Reader VideoProps :> es
    ) => Double -> (Double -> Svg.Document) -> Eff es Svg.Document
animate duration f = animateM duration (pure . f)

