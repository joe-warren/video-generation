module Lib
    ( someFunc
    ) where

import qualified Data.Text.IO as T
import CodeScene (highlightAndSave)

someFunc :: IO ()
someFunc = highlightAndSave "out.svg" =<< T.readFile "src/Lib.hs"
