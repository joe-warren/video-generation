module Object where

-- BLOCK:Intro

-- Lets design a hook
-- Using Haskell, and Waterfall-CAD

-- BLOCK:Imports
-- Imports
import Linear
import Control.Lens
import qualified Waterfall as W 

-- BLOCK:ArrowheadPropertiesDef
data ArrowheadProperties = ArrowheadProperties
    { arrowheadWidth :: Double 
    , arrowheadHeight :: Double 
    , arrowheadNotchDepth :: Double
    , arrowheadAngle :: Double
    }

-- BLOCK:PropertiesDef
data HookProperties = HookProperties 
    { hookRadius :: Double
    , sweepRadius :: Double
    , sweepAngle :: Double
    , hoopRadius :: Double 
    , hoopHeight :: Double
    , nHooks :: Int
    , arrowheadProperties :: ArrowheadProperties
    }


-- BLOCK:Degrees

degrees :: Double -> Double 
degrees = (* (pi / 180))

-- BLOCK:PropertiesValue
properties = HookProperties 
    { hookRadius = 0.25
    , sweepRadius = 1.25
    , sweepAngle = degrees 200
    , hoopRadius = 0.25
    , hoopHeight = 2
    , nHooks = 3
    , arrowheadProperties = ArrowheadProperties 
        { arrowheadWidth = 1
        , arrowheadHeight = 1.2
        , arrowheadNotchDepth = 0.5
        , arrowheadAngle = degrees 0
        }
    }

-- BLOCK: Hoop

hookHoop:: HookProperties -> W.Solid
hookHoop (HookProperties {..})= 
    W.torus (hoopRadius + hookRadius) hookRadius
        & W.rotate (unit _x) (pi/2)
        & W.translate ((hoopHeight + hoopRadius + hookRadius) *^ unit _z)

-- BLOCK: HookShaft     
hookShaft:: HookProperties -> W.Solid
hookShaft (HookProperties {..}) =
    W.unitCylinder 
        & W.scale (V3 hookRadius hookRadius hoopHeight)

        
-- BLOCK: HookCurve
hookCurvePath :: HookProperties -> W.Path
hookCurvePath (HookProperties {..}) = 
    let v = (negate sweepRadius *^ unit _x)
        rotate = W.rotate (negate $ unit _y)
    in W.arcVia v (rotate (sweepAngle/2) v) (rotate sweepAngle v)
        & W.translate (sweepRadius *^ unit _x)

hookCurve :: HookProperties -> W.Solid
hookCurve props@(HookProperties {..}) = 
    W.sweep (hookCurvePath props) (W.unitCircle & W.uScale2D hookRadius)


-- BLOCK: AnimatedHookWire (NotShown)

clamp :: Double -> Double
clamp x | x < 0 = 0
        | x >= 1 = 1
        | otherwise = x

calcFractions :: Double -> [Double] -> [Double]
calcFractions fraction elems = 
    let total = sum elems
        normalized = (/ total) <$> elems
        go frac (x:xs) = clamp (frac / x) : go (frac - x) xs
        go _ [] = []
    in go fraction normalized


singleHookWireAnimation :: HookProperties -> Double -> W.Solid
singleHookWireAnimation props@(HookProperties {..}) fraction = 
    let hoopPath =
            W.unitCircle
                & W.shapePaths
                & mconcat
                & W.fromPath2D
                & W.uScale (hoopRadius + hookRadius)
                & W.rotate (unit _x) (pi/2)
                & W.rotate (unit _y) (pi/2)
                & W.translate ((hoopHeight + hoopRadius + hookRadius) *^ unit _z)
        shaftPath = W.line zero (unit _z ^* hoopHeight)
        curvePath = hookCurvePath props
        [curveF, shaftF, hoopF] = calcFractions fraction $ fmap W.pathLength
            [ curvePath
            , shaftPath
            , hoopPath 
            ]

        profile = (W.unitCircle & W.uScale2D hookRadius)
    in mconcat 
        [ if hoopF == 0 then mempty else (`W.sweep` profile) $ W.takePathFraction hoopF hoopPath
        , (`W.sweep` profile) $ W.takePathFraction curveF (W.reversePath curvePath)
        , W.translate (unit _z ^* 0.01)$ (`W.sweep` profile) $ W.takePathFraction shaftF shaftPath
        ]

-- BLOCK: SingleHookWire

singleHookWire :: HookProperties -> W.Solid
singleHookWire props = mconcat 
    [ hookHoop props
    , hookShaft props
    , hookCurve props
    ]

-- BLOCK:Arrowhead


arrowhead :: Double -> ArrowheadProperties -> W.Solid
arrowhead hookRadius (ArrowheadProperties {..})= 
    -- | set the arrowhead thickness, such that, for a given width,
    -- the edge of the hook is tangent to the surface of the arrowhead 
    let y = arrowheadWidth / 2 
        l = y * arrowheadHeight / (arrowheadHeight + arrowheadNotchDepth)
        theta = asin (hookRadius / l)
        x = hookRadius / (cos theta)
    in W.pointedLoft Nothing
            [ W.closeLoop $ W.pathFrom (x *^ unit _x)
                [ W.lineTo 
                    (y  *^ unit _y 
                    - arrowheadNotchDepth *^ unit _z)
                , W.lineTo (negate (x *^ unit _x))
                ]
            ]
            (Just (arrowheadHeight *^ unit _z))
        & mconcat [id, W.mirror (unit _y)]
        & W.rotate (unit _z) arrowheadAngle

hook :: HookProperties -> W.Solid
hook (HookProperties {..})= mconcat 
    [ W.torus (hoopRadius + hookRadius) hookRadius
        & W.rotate (unit _x) (pi/2)
        & W.translate ((hoopHeight + hoopRadius + hookRadius) *^ unit _z)
    , W.unitCylinder 
        & W.scale (V3 hookRadius hookRadius hoopHeight)
    , let v = (negate sweepRadius *^ unit _x)
          rotate = W.rotate (negate $ unit _y)
          sweepPath = 
                ( let                   
                  in W.arcVia v (rotate (sweepAngle/2) v) (rotate sweepAngle v)
                )
          arrowheadPosition = maybe (error "failed to get pathEndpoints" ) snd $ W.pathEndpoints sweepPath
          positionedArrowhead = 
                arrowhead hookRadius arrowheadProperties
                    & W.rotate (unit _y) (pi-sweepAngle)
                    & W.translate arrowheadPosition
      in W.sweep sweepPath (W.uScale2D hookRadius W.unitCircle )
            & (<> positionedArrowhead)
            & W.translate (sweepRadius *^ unit _x)
            & iterate (W.rotate (unit _z) (pi * 2 / fromIntegral nHooks))
            & take nHooks 
            & mconcat
    ]

hook' :: W.Solid
hook' = hook properties


