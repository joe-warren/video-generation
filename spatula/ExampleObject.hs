module ExampleObject where

-- BLOCK:Intro


-- Lets Design a Spatula
-- Using Haskell, and Waterfall-CAD


-- BLOCK:Imports
-- Imports
import Linear
import Control.Lens
import qualified Waterfall as W 

-- BLOCK:Profile

bladeProfile :: W.Path2D
bladeProfile =
    W.closeLoop $
    W.pathFrom (V2 (-20) 0)
        [ W.lineRelative (V2 40 0)
        , W.lineRelative (V2 10 60)
        , W.arcViaTo (V2 0 70) (V2 (-30) 60)
        ]

-- BLOCK:Sharp Blade

bladeThickness :: Double
bladeThickness = 3

sharpBlade :: W.Solid
sharpBlade =  W.prism bladeThickness (W.makeShape bladeProfile) 

-- BLOCK: Animated Blade (Not shown)

animatedBlade :: Double -> W.Solid
animatedBlade f 
    | f < 1e-3 = sharpBlade
    | otherwise =  W.roundConditionalFillet (roundFn $ f * 10) sharpBlade

-- BLOCK:Blade

roundFn :: Double -> (V3 Double, V3 Double) -> Maybe Double 
roundFn radius (s, e)  | (nearZero (s ^. _xy - e ^. _xy)) 
                    && nearZero (s ^. _y) 
                        = Just radius
                | otherwise = Nothing 

blade :: W.Solid
blade = W.roundConditionalFillet (roundFn 10) sharpBlade

-- BLOCK: Slot Profile

slotProfile :: W.Path2D
slotProfile = W.closeLoop $ W.pathFrom (V2 (-3) 0)
    [ W.arcViaRelative (V2 3 (-3)) (V2 6 0)
    , W.lineRelative (30 *^ unit _y)
    , W.arcViaRelative (V2 (-3) 3) (V2 (-6) 0)
    ]

-- BLOCK: Slot Animations (Not Shown)

growSlot :: Double -> W.Solid
growSlot f 
    | f < 1e-3 = W.emptySolid
    | otherwise = 
        W.translate (V3 0 25 0)
            . W.prism (bladeThickness * f)
            $ W.makeShape slotProfile

sweepSlots :: Double -> W.Solid
sweepSlots f = mconcat
    [ W.translate (V3 (x * 12.5) 25 0)
        . W.rotate (unit _z) (x * (-pi/20))
        . W.prism bladeThickness 
        $ W.makeShape slotProfile
    | x <- [-f, 0, f]
    ]

-- BLOCK: Slotted Blade

slots :: W.Solid
slots = mconcat
    [ W.translate (V3 (x * 12.5) 25 0)
        . W.rotate (unit _z) (x * (-pi/20))
        . W.prism bladeThickness 
        $ W.makeShape slotProfile
    | x <- [-1, 0, 1]
    ]

slottedBlade :: W.Solid
slottedBlade = blade `W.difference` slots

-- BLOCK:Handle Params

handleLongLeg :: V3 Double
handleLongLeg = V3 0 (-60) 15

handleShortLeg :: V3 Double 
handleShortLeg = V3 0 (-30) 30

handleJoinL :: Double 
handleJoinL = 5

-- BLOCK:Handle Path

handlePath :: W.Path
handlePath =
    let shortLegDir = handleJoinL *^ normalize handleShortLeg
        longLegDir = handleJoinL *^ normalize handleLongLeg
    in W.pathFrom (5 *^ unit _y)
            [ W.lineRelative handleShortLeg
            , W.bezierRelative
                shortLegDir
                shortLegDir
                (shortLegDir + longLegDir)
            , W.lineRelative handleLongLeg
            ]

-- BLOCK:Handle

handle :: W.Solid
handle = 
    W.sweep handlePath (W.scale2D (V2 10 7.5) W.centeredSquare)

-- BLOCK:Grip

grip :: W.Solid
grip = 
    let Just (_, e) = W.pathEndpoints handlePath
        gripD = normalize handleLongLeg ^* 30
        gripPath = W.line (e - gripD) (e + gripD)
    in W.sweep gripPath (W.scale2D (V2 16 12) W.centeredSquare)
        & W.roundFillet 4

-- BLOCK: Handle And Grip

handleWithGrip :: W.Solid
handleWithGrip = grip <> handle

-- BLOCK:Hole

hole :: W.Solid
hole = 
    let Just (_, e) = W.pathEndpoints handlePath
        holeD = normalize handleLongLeg
        holePath = W.line (e + holeD ^* 10) (e + holeD ^* 25)
    in W.sweep holePath (W.scale2D (V2 6 30) W.centeredSquare)
        & W.roundFillet 2.75

handleWithHole = handleWithGrip `W.difference` hole

-- BLOCK: Negative Mask

negativeMask :: W.Solid
negativeMask = 
    W.centeredCube 
        & W.translate (-0.5 *^ unit _z)
        & W.uScale 1000

-- BLOCK: Spatula
spatula :: W.Solid
spatula = (slottedBlade <> handleWithHole) `W.difference` negativeMask

-- BLOCK: Outro

-- With Apologies to Guy Steele
