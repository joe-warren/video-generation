module Lib
    ( someFunc
    ) where

import qualified Data.Text.IO as T
import CodeScene (highlightAndSave)

someFunc :: IO ()
someFunc = highlightAndSave "output" =<< T.readFile "src/Lib.hs"
