module Object where

-- BLOCK:Imports

import Linear
import Control.Lens
import qualified Waterfall as W 


-- BLOCK: Single Segment

segmentW = 10

singleSegment :: W.Solid
singleSegment = W.centeredCube 
    & W.uScale segmentW
    & W.chamfer 1.5

-- BLOCK: Single Segment Animated (Not Shown)

singleSegmentStage1 :: Double -> W.Solid
singleSegmentStage1 frac =
    if nearZero frac 
        then mempty
        else W.centeredCube 
            & W.uScale (frac * segmentW)

singleSegmentStage2 :: Double -> W.Solid
singleSegmentStage2 frac = 
    W.centeredCube 
    & W.uScale segmentW
    & (if nearZero frac then id else W.chamfer (1.5 * frac))

-- BLOCK: Combined Segments

segmentGap = 1

centerGap = segmentW + segmentGap


combinedSegments :: W.Solid
combinedSegments = 
    let positions = [-1, 0, 1]
    in mconcat 
        [ singleSegment 
            & W.translate (v ^* centerGap)
        | v <- V3 <$> positions <*> positions <*> positions
        ]

-- BLOCK: Combined Segments Animated (Not Shown)

clamp = max 0 . min 1

combinedSegmentsAnimated :: Double -> W.Solid
combinedSegmentsAnimated t = 
    let positions = [-1, 0, 1]
        segment t' = 
            if t' < 0.5 
                then singleSegmentStage1 . clamp $ (t' * 2)
                else singleSegmentStage2 . clamp $ ((t' - 0.5) * 2 )
    in mconcat 
        [ (t * 2  - (realToFrac index / 27)) -- (t * 5 - (realToFrac index / 27))
            & clamp 
            & segment
            & W.translate (V3 i j k ^* centerGap)
        | (index :: Integer, (i, j, k)) <- zip [0..] [(i, j, k)  | i <- positions, j <- positions, k <- positions]
        ]