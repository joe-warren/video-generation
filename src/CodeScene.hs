module CodeScene where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Skylighting as Sky
import qualified Graphics.Svg as Svg
import qualified Codec.Picture.Types as JP
import Control.Lens
import Data.Maybe (maybe)
import Data.Monoid (Last (..))

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

tokenColour :: Sky.TokenType -> JP.PixelRGBA8
tokenColour tokType =
    let i = fromEnum tokType
        r = fromIntegral (i `rem` 2) * 255
        g = fromIntegral ((i `div` 2) `rem` 2) * 255
        b = fromIntegral ((i `div` 4) `rem` 2) * 255
    in JP.PixelRGBA8 r g b 255

lineToSvg :: Sky.SourceLine -> Svg.Tree
lineToSvg = 
    let textTree t = Svg.TextTree Nothing (Svg.defaultSvg & Svg.textRoot . Svg.spanContent .~ [Svg.SpanText t])
        f _ [] = []
        f offset ((tokenType, t):xs) = 
            let trans = translate (15 * offset) 0
                colour a = a & Svg.drawAttr . Svg.fillColor .~ (Last . Just $ Svg.ColorRef (tokenColour tokenType))
                newOffset  = offset + fromIntegral (T.length t)
            in (colour . trans $ (textTree t)) : f newOffset xs  
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