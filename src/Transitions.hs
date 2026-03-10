module Transitions where

import Linear

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