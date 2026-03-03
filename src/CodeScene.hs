module CodeScene where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Skylighting as Sky
import qualified Graphics.Svg as Svg
import qualified Codec.Picture.Types as JP
import Control.Lens
import Control.Monad (join, forM)
import Data.Monoid (Last (..))
import Control.Arrow (second)
import Numeric (showFFloat)

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

colour :: Svg.WithDrawAttributes a => JP.PixelRGBA8 -> a -> a    
colour c a = a & Svg.drawAttr . Svg.fillColor .~ (Last . Just $ Svg.ColorRef c)

tokenColour :: Sky.TokenType -> JP.PixelRGBA8
tokenColour tokType =
    let i = fromEnum tokType
        r = fromIntegral (i `rem` 2) * 255
        g = fromIntegral ((i `div` 2) `rem` 2) * 255
        b = fromIntegral ((i `div` 4) `rem` 2) * 255
    in JP.PixelRGBA8 r g b 255

data LetterType = StartOrMidWord | EndOfWord | EndOfLine | EndOfFile

lineToSvg :: Sky.SourceLine -> [(LetterType, Svg.Tree)]
lineToSvg = 
    let f _ [] = []
        f offset ((_, ' '):xs) = f (offset+1) xs
        f offset ((tokenType, t):xs) =
            let trans = translate (10 * offset) 0
                font a = a 
                    & Svg.drawAttr . Svg.fontFamily .~ (pure ["Share Tech Mono"])
                    & Svg.drawAttr . Svg.fontSize .~ (pure $ Svg.Px 20)
                col = colour (tokenColour tokenType)
                newOffset  = offset + 1
                letterType = case xs of
                    (_,' '):_ -> EndOfWord
                    (_,_):_ -> StartOrMidWord
                    [] -> EndOfLine
                textTree = Svg.TextTree Nothing (Svg.defaultSvg & Svg.textRoot . Svg.spanContent .~ [Svg.SpanText . T.singleton $ t])
            in (letterType, font . col . trans $ textTree) : f newOffset xs
        splitChars = ((traverse T.unpack) =<<)
     in f 0 . splitChars

linesToSvg :: [Sky.SourceLine] -> [(LetterType, Svg.Document)]
linesToSvg lines = 
    let w = Svg.Num $ 1024
        h = Svg.Num $ 800
        transform (i, elems) =  second (translate 0 (25 * i)) <$> elems
        elems = lines
            & fmap lineToSvg
            & zip [1..]
            & fmap transform
            & join
            & set  (_last . _1) EndOfFile
        background = Svg.RectangleTree $ 
            Svg.defaultSvg 
                & Svg.rectUpperLeftCorner .~ (Svg.Px 0, Svg.Px 0)
                & Svg.rectWidth .~ w
                & Svg.rectHeight .~ h
                & colour (JP.PixelRGBA8 255 255 255 255)

        makePages _ [] = []
        makePages prev ((letterType, letter):xs) = 
            let group = Svg.GroupTree $ Svg.Group mempty (background : letter : prev) Nothing Svg.defaultSvg
                document = Svg.Document Nothing (Just w) (Just h) [group] mempty mempty mempty mempty
            in (letterType, document) : makePages (letter:prev) xs
        in makePages [] elems

durations :: LetterType -> Int
durations StartOrMidWord = 1
durations EndOfWord = 3
durations EndOfLine = 15
durations EndOfFile = 100

writeConcatFile :: FilePath -> [(Int, FilePath)] -> IO ()
writeConcatFile filepath= 
    let showFloat f = T.pack $ showFFloat Nothing f [] 
        go (nFrames, path) = replicate nFrames
            ("file '" <> T.pack path <> "'\n" <>
            "duration " <> showFloat 0.04 <> "\n")
    in T.writeFile filepath . T.concat . (go =<<)

highlightAndSave :: FilePath -> Text -> IO ()
highlightAndSave dir text = do
    let frames = linesToSvg . highlightHaskell $ text
    framesAndDurations <- forM (zip [0..] frames) $ \(i, (letterType, frame)) -> do
            let path = (dir <> "/" <> show i <> ".svg")
            Svg.saveXmlFile path frame
            return (durations letterType, path)
    writeConcatFile "output.txt" framesAndDurations
