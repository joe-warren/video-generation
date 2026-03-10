module Lib
    ( someFunc
    ) where

import qualified Data.Text.IO as T
import qualified CodeScene 
import qualified CodeBlock
import GenerateVideo (runBuildVideo, runWriteFrames)
import VideoProps 
import qualified ExampleObject
import qualified WaterfallScene
import qualified Waterfall as W

import Effectful.Reader.Static
import Effectful
import Data.Default (def)
import Control.Lens
import Linear

videoProps :: VideoProps
videoProps = VideoProps 
    { _videoWidth = 1920
    , _videoHeight = 1080
    , _videoFPS = 25
    , _videoScratchDir = "output"
    , _videoOutputFile = "output.mp4"
    } 

run =
    runEff 
    . CodeBlock.runLoadCodeBlocks'
    . CodeScene.runHighlight
    . runReader videoProps
    . runBuildVideo
    . runWriteFrames

easeInOutElastic :: Double -> Double 
easeInOutElastic x
    | nearZero x = 0
    | nearZero (x-1) = 1
    | x < 0.5   = (2 ** (  20  * x - 10)) * sin ( (20 * x - 11.125) * c5) / 2 
    | otherwise = (2 ** ((-20) * x + 10)) * sin ( (20 * x - 11.125) * c5) / 2 + 1 
    where c5 = (2 * pi) / 4.5

easeInOutBack :: Double -> Double 
easeInOutBack x 
    | x < 0.5 =    ((2 * x)     ** 2 ) * ((c2 + 1) * (2 * x    ) - c2) / 2
    | otherwise = (((2 * x - 2 ) ** 2) * ((c2 + 1) * (x * 2 - 2) + c2) + 2) / 2
    where 
        c1 = 1.70158
        c2 = c1 * 1.525

easeInOutSin :: Double -> Double
easeInOutSin x = (1 - cos (pi * x)) / 2 

easeInOutSoft :: Double -> Double
easeInOutSoft x 
    | x < fac = easeInOutBack (x / fac)-- (easeInOutSin x + x) / 2 
    | otherwise = 1
    where fac = 0.9

someFunc :: IO ()
someFunc = run $ do
    CodeScene.codeScene def 
        =<< CodeBlock.loadCodeBlock "src/ExampleObject.hs" "Intro"
    CodeScene.codeScene def  
        =<< CodeBlock.loadCodeBlock "src/ExampleObject.hs" "Imports"
    CodeScene.codeScene def  
        =<< CodeBlock.loadCodeBlock "src/ExampleObject.hs" "Profile"
    WaterfallScene.stillClip 2 
        (ExampleObject.bladeProfile 
            & W.pathDiagram W.OutLine W.Visible
            & W.uScale2D (1/80)
            & WaterfallScene.centerDiagram
        )
        
    CodeScene.codeScene def  
        =<< CodeBlock.loadCodeBlock "src/ExampleObject.hs" "Sharp Blade"

    let center solid = 
            case W.axisAlignedBoundingBox solid of
                Just (lo, hi) -> W.translate ((lo + hi) ^* (-0.5)) solid
                Nothing -> solid

    let showRotating' axis scale angle solid t = solid
            & W.uScale scale 
            & W.rotate axis (angle * t)
            & W.solidDiagram (V3 2 2 1)
        showRotating = showRotating' (unit _z)

    WaterfallScene.animatedClip 4 $ showRotating (1/150) pi  
        (ExampleObject.sharpBlade & center)  
        . easeInOutSoft

    CodeScene.codeScene def  
        =<< CodeBlock.loadCodeBlock "src/ExampleObject.hs" "Blade"
    
    WaterfallScene.animatedClip 4 $ showRotating  (1/150) pi 
        ( ExampleObject.blade & center
        ) . easeInOutSoft

    CodeScene.codeScene def  
        =<< CodeBlock.loadCodeBlock "src/ExampleObject.hs" "Handle Path"
        
    CodeScene.codeScene def  
        =<< CodeBlock.loadCodeBlock "src/ExampleObject.hs" "Handle"
        
    WaterfallScene.animatedClip 4 $ showRotating  (1/150) pi 
        ( ExampleObject.handle & center
        ) . easeInOutSoft

    CodeScene.codeScene def  
        =<< CodeBlock.loadCodeBlock "src/ExampleObject.hs" "Grip"
        
    WaterfallScene.animatedClip 4 $ showRotating  (1/150) pi 
        ( ExampleObject.grip & center
        ) . easeInOutSoft

    CodeScene.codeScene def  
        =<< CodeBlock.loadCodeBlock "src/ExampleObject.hs" "Handle And Grip"
        
    WaterfallScene.animatedClip 4 $ showRotating (1/150) pi 
        ( ExampleObject.handleWithGrip & center
        ) . easeInOutSoft

    CodeScene.codeScene def  
        =<< CodeBlock.loadCodeBlock "src/ExampleObject.hs" "Hole"
        
    WaterfallScene.animatedClip 4 $ showRotating (1/150) pi 
        ( ExampleObject.handleWithHole & center
        ) . easeInOutSoft
        
    CodeScene.codeScene def  
        =<< CodeBlock.loadCodeBlock "src/ExampleObject.hs" "Negative Mask"

    CodeScene.codeScene def  
        =<< CodeBlock.loadCodeBlock "src/ExampleObject.hs" "Spatula"

    WaterfallScene.animatedClip 8 $ showRotating (1/150) (2*pi)
            ( ExampleObject.spatula 
                & center
            ) . easeInOutSoft

    WaterfallScene.animatedClip 8 $ showRotating' (unit _x) (1/150) (2*pi)
            ( ExampleObject.spatula 
                & center
            ) . easeInOutSoft

