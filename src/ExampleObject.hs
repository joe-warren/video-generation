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

-- BLOCK:Blade

roundFn :: (V3 Double, V3 Double) -> Maybe Double 
roundFn (s, e)  | (nearZero (s ^. _xy - e ^. _xy)) 
                    && nearZero (s ^. _y) 
                        = Just 10
                | otherwise = Nothing 

blade :: W.Solid
blade = W.roundConditionalFillet roundFn sharpBlade

-- BLOCK:Handle Path

handleLongLeg :: V3 Double
handleLongLeg = V3 0 (-60) 15

handlePath :: W.Path
handlePath =
    let shortLeg = V3 0 (-30) 30
        joinL = 5
    in W.pathFrom (5 *^ unit _y)
            [ W.lineRelative shortLeg
            , W.bezierRelative
                (joinL *^ normalize shortLeg) 
                (joinL *^ normalize shortLeg)
                (joinL *^ (normalize shortLeg + normalize handleLongLeg))
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
spatula = (blade <> handleWithGrip) `W.difference` negativeMask
