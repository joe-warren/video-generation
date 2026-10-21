module Object where

-- BLOCK:Imports

import Linear
import Control.Lens
import qualified Waterfall as W 
import Data.Maybe (fromMaybe)


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

combinedSegmentW = 3 * segmentW + 2 * segmentGap

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

-- BLOCK: Single Sticker 

stickerW = segmentW - 3.5

singleSticker :: W.Solid 
singleSticker = W.centeredCube 
    & W.scale (V3 stickerW stickerW 0.5)
    & W.roundConditionalFillet (W.whenEndpointsNearlyEqual _xy 1)

-- BLOCK: Sticker Animated (not shown)

singleStickerStage1 :: Double -> W.Solid
singleStickerStage1 f =
    if f < 0.01 
        then mempty 
        else let w = f * stickerW
            in W.centeredCube 
                & W.scale (V3 w w 0.5)

singleStickerStage2 :: Double -> W.Solid
singleStickerStage2 f = W.centeredCube 
    & W.scale (V3 stickerW stickerW 0.5)
    & (if f < 0.01 
        then id 
        else W.roundConditionalFillet (W.whenEndpointsNearlyEqual _xy (1 * f))
    )

-- BLOCK: One Side Of Stickers

oneSideOfStickers :: W.Solid
oneSideOfStickers = 
    let positions = [-1, 0, 1]
    in mconcat 
        [ singleSticker 
            & W.translate ((zero & _xy .~ v)  ^* centerGap)
        | v <- V2 <$> positions <*> positions 
        ]

-- BLOCK: One Side Of Stickers Animated (Not Shown)

oneSideOfStickersAnimated :: Double -> W.Solid
oneSideOfStickersAnimated t = 
    let positions = [-1, 0, 1]
        segment t' = 
            if t' < 0.5 
                then singleStickerStage1 . clamp $ (t' * 2)
                else singleStickerStage2 . clamp $ ((t' - 0.5) * 2 )
    in mconcat 
        [ (t * 2  - (realToFrac index / 9)) 
            & clamp 
            & segment
            & W.translate ((zero & _xy .~ v) ^* centerGap)
        | (index :: Integer, v) <- zip [0..] (V2 <$> positions <*> positions)
        ]

-- BLOCK: Segments With Stickers

segmentsWithStickers :: W.Solid
segmentsWithStickers = mconcat $ 
    combinedSegments : 
        [ oneSideOfStickers
            & W.translate (h ^* (combinedSegmentW/2))
            & W.rotate axis (pi/2)
            | h <- [unit _z, negate $ unit _z]
            , axis <- [unit _x, unit _y, unit _z]
        ]

-- BLOCK: Segments With Stickers Animated (Not Shown)

segmentsWithStickersAnimated :: Double -> W.Solid
segmentsWithStickersAnimated f = mconcat $ 
    combinedSegments : 
        [  (f * 3 - realToFrac index / 3) 
            & clamp
            & oneSideOfStickersAnimated
            & W.translate (h ^* (combinedSegmentW/2))
            & W.rotate axis (pi/2)
            | h <- [unit _z, negate $ unit _z]
            , (index, axis) <- zip [0..] [unit _x, unit _y, unit _z]
        ]


-- BLOCK: Segments With Stickers With Colour

colours = [W.yellow, W.aqua, W.lime, W.red, W.fuchsia, W.white]

segmentsWithStickersWithColour :: W.Solid
segmentsWithStickersWithColour = mconcat $ 
    W.colourSolid W.navy combinedSegments : 
        [ oneSideOfStickers
            & W.translate (h ^* (combinedSegmentW/2))
            & W.rotate axis (pi/2)
            & W.colourSolid col
            | (col, (h, axis)) <- 
                zip colours
                ((,) <$> [unit _z, negate $ unit _z]
                    <*> [unit _x, unit _y, unit _z])
        ]