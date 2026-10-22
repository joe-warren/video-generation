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
    , Reader VideoProps :> es
    ) => Double -> (Double -> Eff es Svg.Document) -> Eff es Svg.Document
animateM duration f = do
    vd <- ask
    let nFrames = max 1 . round $ (duration * fromInteger (vd ^. videoFPS))
        frameDuration = duration / fromInteger nFrames

    let v = (/ fromInteger (max 1 (nFrames - 1))) . fromInteger
    traverse_ (addSvgDuration frameDuration <=< f . v) [0..nFrames-1]
    f 1

animate ::
    ( BuildVideo :> es
    , Reader VideoProps :> es
    ) => Double -> (Double -> Svg.Document) -> Eff es Svg.Document
animate duration f = animateM duration (pure . f)

