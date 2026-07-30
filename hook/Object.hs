module Object where

-- BLOCK:Intro

-- In 2018, I designed a parametric hook
-- 
-- I used OpenSCAD, a Programmable CAD framework

-- BLOCK:Imports

-- Since then, I've built my own library for Programmable CAD
--
-- It's called Waterfall-CAD
--
-- It's written in Haskell, a purely functional programming language 
-- 
-- Using Haskell and Waterfall-CAD, let's reimplement the hook

import Linear
import Control.Lens
import qualified Waterfall as W 


-- BLOCK:Properties Def
data HookProperties = HookProperties 
    { hookRadius :: Double
    , sweepRadius :: Double
    , sweepAngle :: Double
    , hoopRadius :: Double 
    , hoopHeight :: Double
    , nHooks :: Int
    , arrowheadProperties :: ArrowheadProperties
    }

data ArrowheadProperties = ArrowheadProperties
    { arrowheadWidth :: Double 
    , arrowheadHeight :: Double 
    , arrowheadNotchDepth :: Double
    , arrowheadAngle :: Double
    }


-- BLOCK:Degrees

degrees :: Double -> Double 
degrees = (* (pi / 180))

-- BLOCK:Properties Value
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

-- BLOCK: Hoop and Shaft

hookHoop :: HookProperties -> W.Solid
hookHoop (HookProperties {..}) = 
    W.torus (hoopRadius + hookRadius) hookRadius
        & W.rotate (unit _x) (pi/2)
        & W.translate ((hoopHeight + hoopRadius + hookRadius) *^ unit _z)

hookShaft :: HookProperties -> W.Solid
hookShaft (HookProperties {..}) =
    W.unitCylinder 
        & W.scale (V3 hookRadius hookRadius hoopHeight)

        
-- BLOCK: Hook Curve
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
        , W.translate (unit _z ^* 0.001)$ (`W.sweep` profile) $ W.takePathFraction shaftF shaftPath
        ]

-- BLOCK: Single Hook Wire

singleHookWire :: HookProperties -> W.Solid
singleHookWire props = mconcat 
    [ hookHoop props
    , hookShaft props
    , hookCurve props
    ]

-- BLOCK:Half Arrowhead

halfArrowhead :: Double -> ArrowheadProperties -> W.Solid
halfArrowhead hookRadius (ArrowheadProperties {..}) = 
    -- set the arrowhead thickness, such that for a given width,
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

-- BLOCK: defaultHalfArrowhead (notShown)

defaultHalfArrowhead :: W.Solid
defaultHalfArrowhead = halfArrowhead (hookRadius properties) (arrowheadProperties properties)

-- BLOCK: Arrowhead

arrowhead :: Double -> ArrowheadProperties -> W.Solid
arrowhead hookRadius props@(ArrowheadProperties {..}) = 
    halfArrowhead hookRadius props 
        & mconcat [id, W.mirror (unit _y)]
        & W.rotate (unit _z) arrowheadAngle

        
-- BLOCK: Arrowhead Animation (NotShown)

growFromBottom :: W.Solid -> Double -> W.Solid
growFromBottom s fraction =
    case W.axisAlignedBoundingBox s of 
        Nothing -> mempty
        Just (lo, hi) -> 
            W.intersection s (W.aabbToSolid (lo, hi & _z .~ ((lo + ((hi-lo) ^* fraction)) ^. _z )))


defaultArrowheadAnimation :: Double -> W.Solid
defaultArrowheadAnimation fraction = 
    halfArrowhead (hookRadius properties) (arrowheadProperties properties)
        & mconcat [id, (`growFromBottom` fraction) . W.mirror (unit _y)]


-- BLOCK: Single Hook With Arrowhead

singleHookWithArrowhead :: HookProperties -> W.Solid
singleHookWithArrowhead props@(HookProperties {..}) = mconcat 
    [ hookHoop props
    , hookShaft props
    , hookCurve props
    , let arrowheadPosition = 
            maybe (error "failed to get pathEndpoints" ) snd 
                $ W.pathEndpoints (hookCurvePath props)
      in arrowhead hookRadius arrowheadProperties
            & W.rotate (unit _y) (pi-sweepAngle)
            & W.translate arrowheadPosition
    ]

    
-- BLOCK: Single Hook With Arrowhead Animation (Not Shown)

singleHookWithArrowheadAnimation :: HookProperties -> Double ->  W.Solid
singleHookWithArrowheadAnimation props@(HookProperties {..}) fraction = mconcat 
    [ hookHoop props
    , hookShaft props
    , hookCurve props
    , let arrowheadPosition = 
            maybe (error "failed to get pathEndpoints" ) snd 
                $ W.pathEndpoints (hookCurvePath props)
      in arrowhead hookRadius arrowheadProperties
            & (`growFromBottom` fraction)
            & W.rotate (unit _y) (pi-sweepAngle)
            & W.translate arrowheadPosition
    ]

    
-- BLOCK: Whole Hook

hook :: HookProperties -> W.Solid
hook props@(HookProperties {..}) = mconcat 
    [ hookHoop props
    , hookShaft props
    , let arrowheadPosition = 
            maybe (error "failed to get pathEndpoints" ) snd 
                $ W.pathEndpoints (hookCurvePath props)
      in arrowhead hookRadius arrowheadProperties
            & W.rotate (unit _y) (pi-sweepAngle)
            & W.translate arrowheadPosition
            & (<> hookCurve props)
            & iterate (W.rotate (unit _z) (pi * 2 / fromIntegral nHooks))
            & take nHooks 
            & mconcat
    ]

-- BLOCK: Animate whole Hook (Not Shown)

animateInHook :: HookProperties -> Double -> W.Solid
animateInHook props@(HookProperties {..}) fraction =
    if fraction <= 1e-2
        then singleHookWithArrowhead props 
        else mconcat 
            [ hookHoop props
            , hookShaft props
            , let arrowheadPosition = 
                    maybe (error "failed to get pathEndpoints" ) snd 
                        $ W.pathEndpoints (hookCurvePath props)
              in arrowhead hookRadius arrowheadProperties
                    & W.rotate (unit _y) (pi-sweepAngle)
                    & W.translate arrowheadPosition
                    & (<> hookCurve props)
                    & iterate (W.rotate (unit _z) (pi * 2 * fraction / fromIntegral nHooks))
                    & take nHooks
                    & mconcat
            ]

-- BLOCK: Properties Rotated Head

propertiesRotatedHead = HookProperties
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
        , arrowheadAngle = degrees 90
        }
    }

-- BLOCK: Properties Five Hooks

propertiesFiveHooks = HookProperties
    { hookRadius = 0.25
    , sweepRadius = 1.25
    , sweepAngle = degrees 200
    , hoopRadius = 0.25
    , hoopHeight = 2
    , nHooks = 5
    , arrowheadProperties = ArrowheadProperties 
        { arrowheadWidth = 1
        , arrowheadHeight = 1.2
        , arrowheadNotchDepth = 0.5
        , arrowheadAngle = degrees 0
        }
    }

-- BLOCK: Properties High Sweep Angle

propertiesHighSweepAngle = HookProperties
    { hookRadius = 0.25
    , sweepRadius = 1.25
    , sweepAngle = degrees 240
    , hoopRadius = 0.25
    , hoopHeight = 2
    , nHooks = 5
    , arrowheadProperties = ArrowheadProperties 
        { arrowheadWidth = 1
        , arrowheadHeight = 1.2
        , arrowheadNotchDepth = 0.5
        , arrowheadAngle = degrees 0
        }
    }

-- BLOCK: Properties Low Sweep Angle

propertiesLowSweepAngle = HookProperties
    { hookRadius = 0.25
    , sweepRadius = 1.25
    , sweepAngle = degrees 90
    , hoopRadius = 0.25
    , hoopHeight = 2
    , nHooks = 5
    , arrowheadProperties = ArrowheadProperties 
        { arrowheadWidth = 1
        , arrowheadHeight = 1.2
        , arrowheadNotchDepth = 0.5
        , arrowheadAngle = degrees 0
        }
    }

    
-- BLOCK: Properties Chonky

propertiesChonky = HookProperties
    { hookRadius = 0.75
    , sweepRadius = 1.25
    , sweepAngle = degrees 180
    , hoopRadius = 0.25
    , hoopHeight = 1
    , nHooks = 3
    , arrowheadProperties = ArrowheadProperties 
        { arrowheadWidth = 4
        , arrowheadHeight = 1.2
        , arrowheadNotchDepth = 0.5
        , arrowheadAngle = degrees 0
        }
    }

-- BLOCK: Interpolated Properties

interpolate :: Double -> Double -> Double -> Double
interpolate t a b = (1 - t) * a + t * b

interpolateArrowhead :: Double -> ArrowheadProperties -> ArrowheadProperties -> ArrowheadProperties
interpolateArrowhead t a b =
    let i f = interpolate t (f a) (f b)
    in ArrowheadProperties
        { arrowheadWidth = i arrowheadWidth
        , arrowheadHeight = i arrowheadHeight
        , arrowheadNotchDepth = i arrowheadNotchDepth
        , arrowheadAngle = i arrowheadAngle
        }

interpolateProperties :: Double -> HookProperties -> HookProperties -> HookProperties
interpolateProperties t a b =
    let i f = interpolate t (f a) (f b)
    in HookProperties
        { hookRadius = i hookRadius
        , sweepRadius = i sweepRadius
        , sweepAngle = i sweepAngle
        , hoopRadius = i hoopRadius
        , hoopHeight = i hoopHeight
        , nHooks = max (nHooks a) (nHooks b)
        , arrowheadProperties =
            interpolateArrowhead t (arrowheadProperties a) (arrowheadProperties b)
        }

-- BLOCK: Interpolated Hook

hookAngles :: Int -> Int -> Double -> [Double]
hookAngles nA nB t =
    let n = max nA nB
        angle m i = 2 * pi * fromIntegral ((i * m) `div` n) / fromIntegral m
        blend i = interpolate t (angle nA i) (angle nB i)
        groupOf m i = [ blend j | j <- [0 .. n - 1], angle m j == angle m i ]
        minSeparation = degrees 5
        snap m i a
            | let g = groupOf m i
            , length g > 1 && maximum g - minimum g < minSeparation = angle m i
            | otherwise = a
    in [ snap nA i . snap nB i $ blend i | i <- [0 .. n - 1] ]

interpolatedHook :: HookProperties -> HookProperties -> Double -> W.Solid
interpolatedHook propsA propsB t =
    let props = interpolateProperties t propsA propsB
        arrowheadPosition =
            maybe (error "failed to get pathEndpoints" ) snd
                $ W.pathEndpoints (hookCurvePath props)
        single = arrowhead (hookRadius props) (arrowheadProperties props)
            & W.rotate (unit _y) (pi - sweepAngle props)
            & W.translate arrowheadPosition
            & (<> hookCurve props)
    in mconcat
        [ hookHoop props
        , hookShaft props
        , mconcat
            [ single & W.rotate (unit _z) a
            | a <- hookAngles (nHooks propsA) (nHooks propsB) t
            ]
        ]

-- BLOCK: Printed

-- I 3D printed it
--
-- Links to the model, and to the source code, are in the video description
--
-- Thanks for watching