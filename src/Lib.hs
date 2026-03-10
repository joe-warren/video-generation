module Lib
    ( someFunc
    ) where

import qualified Data.Text.IO as T
import qualified CodeScene 
import qualified CodeBlock
import GenerateVideo (runBuildVideo, runWriteFrames)
import VideoProps 
import qualified Transitions

import qualified ExampleObject
import qualified WaterfallScene
import qualified Waterfall as W

import Effectful.Reader.Static
import Effectful
import Data.Default (def)
import Control.Lens
import Linear

videoProps :: VideoProps
videoProps = def

run =
    runEff 
    . CodeBlock.runLoadCodeBlocks'
    . CodeScene.runHighlight
    . runReader videoProps
    . runBuildVideo
    . runWriteFrames

easeInOutSoft :: Double -> Double
easeInOutSoft x 
    | x < fac = Transitions.easeInOutBack (x / fac)-- (easeInOutSin x + x) / 2 
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

