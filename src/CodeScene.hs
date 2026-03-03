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
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

width :: Integer
width = 1024

height :: Integer 
height = 800

columns :: Integer 
columns = 80

borderColumns :: Integer
borderColumns = 2

charWidth :: Integer
charWidth = width `div` (columns + 2 * borderColumns)

charHeight :: Integer
charHeight = charWidth * 2

lineHeight :: Integer 
lineHeight = charWidth * 3

style :: Sky.Style
style = Sky.haddock

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

strokeColour :: Svg.WithDrawAttributes a => JP.PixelRGBA8 -> a -> a    
strokeColour c a = a & Svg.drawAttr . Svg.strokeColor .~ (Last . Just $ Svg.ColorRef c)

strokeWidth :: Svg.WithDrawAttributes a => Double -> a -> a    
strokeWidth w a = a & Svg.drawAttr . Svg.strokeWidth .~ (Last . Just . Svg.Px $ w)

convertColor :: Sky.Color -> JP.PixelRGBA8
convertColor (Sky.RGB r g b) = JP.PixelRGBA8 r g b 255

tokenColour :: Sky.TokenType -> JP.PixelRGBA8
tokenColour tokType =
    let tokStyle = tokType `M.lookup` Sky.tokenStyles style
        tokCol = fromMaybe (Sky.RGB 0 0 0) (Sky.tokenColor =<< tokStyle)
        
    in convertColor tokCol

data LetterType = StartOrMidWord | EndOfWord | EndOfLine | EndOfFile

lineToSvg :: Sky.SourceLine -> [(LetterType, Svg.Tree)]
lineToSvg = 
    let f _ [] = []
        f offset ((_, ' '):xs) = f (offset+1) xs
        f offset ((tokenType, t):xs) =
            let trans = translate (fromIntegral charWidth * offset) 0
                font a = a 
                    & Svg.drawAttr . Svg.fontFamily .~ (pure ["Share Tech Mono"])
                    & Svg.drawAttr . Svg.fontSize .~ (pure . Svg.Px . fromIntegral $ charHeight)
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
    let w = Svg.Num . fromIntegral $ width
        h = Svg.Num .fromIntegral $ height

        xOff = fromIntegral $ charWidth * borderColumns
        yOff = fromIntegral ((height - fromIntegral (length lines) * lineHeight) `div` 2)

        transform (i, elems) =  second (translate xOff (yOff + fromIntegral lineHeight * i)) <$> elems
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

        frame = Svg.RectangleTree $ 
            Svg.defaultSvg 
                & Svg.rectUpperLeftCorner .~ 
                    ( Svg.Px $ (fromIntegral borderColumns - 0.5) * fromIntegral charWidth
                    , Svg.Px . fromInteger $ ((height - fromIntegral (length lines) * lineHeight) `div` 2)
                    )
                & Svg.rectWidth .~ (Svg.Px $ (fromIntegral (columns * charWidth)))
                & Svg.rectHeight .~ (Svg.Px $ fromIntegral (length lines + 1) * fromIntegral lineHeight)
                & Svg.drawAttr . Svg.fillColor .~ (pure Svg.FillNone)
                & strokeColour (JP.PixelRGBA8 0 0 0 255)
                & strokeWidth 2

        makePages _ [] = []
        makePages prev ((letterType, letter):xs) = 
            let group = Svg.GroupTree $ Svg.Group mempty (background : frame : letter : prev) Nothing Svg.defaultSvg
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
