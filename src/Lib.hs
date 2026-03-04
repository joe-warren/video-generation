module Lib
    ( someFunc
    ) where

import qualified Data.Text.IO as T
import CodeScene (highlightAndSave)
import ConcatFileVideo (generateVideo)

someFunc :: IO ()
someFunc = generateVideo "output" "output.mp4" 
            =<< highlightAndSave "output"
            =<< T.readFile "src/Lib.hs"
