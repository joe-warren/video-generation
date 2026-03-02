module CodeScene where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Skylighting as Sky
import qualified Graphics.Svg as Svg
import Control.Lens
import Data.Maybe (maybe)

highlightHaskell :: Text -> [Sky.SourceLine]
highlightHaskell text = 
    let tokenizerConfig = Sky.TokenizerConfig Sky.defaultSyntaxMap False
        [syntax] = Sky.syntaxesByExtension Sky.defaultSyntaxMap "hs" 
        Right lines = Sky.tokenize tokenizerConfig syntax text
    in lines

translate :: Svg.WithDrawAttributes a => Double -> Double -> a -> a
translate x y elem = 
    let t = Svg.Translate x y
        addT = Just . maybe [t] (t:)
    in elem & Svg.drawAttr . Svg.transform %~ addT

lineToSvg :: Sky.SourceLine -> Svg.Tree
lineToSvg = 
    let textTree t = Svg.TextTree Nothing (Svg.defaultSvg & Svg.textRoot . Svg.spanContent .~ [Svg.SpanText t])
        f _ [] = []
        f offset ((tokenType, t):xs) = (translate (15 * offset) 0 (textTree t)) : f (offset + fromIntegral (T.length t)) xs  
        group children = Svg.GroupTree $ Svg.Group mempty children Nothing Svg.defaultSvg
     in group . f 0

linesToSvg :: [Sky.SourceLine] -> Svg.Document
linesToSvg lines = 
    let w = Just . Svg.Num $ 1024
        h = Just . Svg.Num $ 800
        group children =  Svg.GroupTree $ Svg.Group mempty children Nothing Svg.defaultSvg
        transform (i, elem) =  translate 0 (20 * i) elem
        e = group . fmap transform . zip [1..] . fmap lineToSvg $ lines
    in Svg.Document Nothing w h [e] mempty mempty mempty mempty

highlightCode :: Text -> Svg.Document
highlightCode = linesToSvg . highlightHaskell  

highlightAndSave :: FilePath -> Text -> IO ()
highlightAndSave filepath = Svg.saveXmlFile filepath . highlightCode