module ExampleObject where

import qualified Waterfall as W
import Linear (V3 (..), (^*), unit, _x, _y )
import Data.Function ((&))

csgExample :: W.Solid
csgExample = let 
    sphere = W.unitSphere
    sphere' = W.translate (unit _x) . W.uScale 0.1 $ sphere
    cube = W.uScale 1.5 W.centeredCube
    cylinder = W.centeredCylinder
         & W.scale (V3 0.55 0.55 4) 
    cylinderA = W.rotate (unit _x) (pi/2) cylinder
    cylinderB = W.rotate (unit _y) (pi/2) cylinder
    object = (cube `W.intersection` sphere) `W.difference` (cylinder `W.union` cylinderA `W.union` cylinderB)
  in object <> sphere'