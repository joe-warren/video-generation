module CodeScene where

import VideoData
import SvgUtils
import GenerateVideo (BuildVideo, WriteFrames, addSvgDuration)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Skylighting as Sky
import qualified Graphics.Svg as Svg
import qualified Codec.Picture.Types as JP
import Control.Lens
import Control.Monad (join, forM_)
import Data.Monoid (Last (..))
import Control.Arrow (second)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Effectful
import Effectful.Reader.Static

columns :: Integer 
columns = 80

borderColumns :: Integer
borderColumns = 2

charWidth :: VideoData -> Integer
charWidth (VideoData {..}) = videoWidth `div` (columns + 2 * borderColumns)

charHeight :: VideoData -> Integer
charHeight vd = charWidth vd * 2

lineHeight :: VideoData -> Integer 
lineHeight vd = (charWidth vd * 5) `div` 2

style :: Sky.Style
style = Sky.haddock

highlightHaskell :: Text -> [Sky.SourceLine]
highlightHaskell text = 
    let tokenizerConfig = Sky.TokenizerConfig Sky.defaultSyntaxMap False
        [syntax] = Sky.syntaxesByExtension Sky.defaultSyntaxMap "hs" 
        Right lines = Sky.tokenize tokenizerConfig syntax text
    in lines

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

lineToSvg :: VideoData -> Sky.SourceLine -> [(LetterType, Svg.Tree)]
lineToSvg vd = 
    let f _ [] = []
        f offset ((_, ' '):xs) = f (offset+1) xs
        f offset ((tokenType, t):xs) =
            let trans = translate (fromIntegral (charWidth vd) * offset) 0
                font a = a 
                    & Svg.drawAttr . Svg.fontFamily .~ (pure ["Share Tech Mono"])
                    & Svg.drawAttr . Svg.fontSize .~ (pure . Svg.Px . fromIntegral . charHeight $ vd)
                col = colour (tokenColour tokenType)
                newOffset  = offset + 1
                letterType = case xs of
                    (_,' '):_ -> EndOfWord
                    (_,_):_ -> StartOrMidWord
                    [] -> EndOfLine
                textTree = Svg.TextTree Nothing 
                    (Svg.defaultSvg 
                        & Svg.textRoot . Svg.spanContent .~ [Svg.SpanText . T.singleton $ t]
                    )
            in (letterType, font . col . trans $ textTree) : f newOffset xs
        splitChars = ((traverse T.unpack) =<<)
     in f 0 . splitChars

linesToSvg :: VideoData -> [Sky.SourceLine] -> [(LetterType, Svg.Document)]
linesToSvg vd@(VideoData {..}) lines = 
    let w = Svg.Num . fromIntegral $ videoWidth
        h = Svg.Num .fromIntegral $ videoHeight

        xDelta = fromIntegral $ (videoWidth - (charWidth vd * (columns + 2 * borderColumns))) `div` 2
        xOff = xDelta + (fromIntegral $ charWidth vd * borderColumns)
        yOff = fromIntegral ((videoHeight - fromIntegral (length lines) * lineHeight vd) `div` 2)

        transform (i, elems) =  second (translate xOff (yOff + fromIntegral (lineHeight vd) * i)) <$> elems
        elems = lines
            & fmap (lineToSvg vd)
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
                    ( Svg.Px $ xDelta + (fromIntegral borderColumns - 0.5) * fromIntegral (charWidth vd)
                    , Svg.Px . fromInteger $ ((videoHeight - fromIntegral (length lines) * lineHeight vd) `div` 2)
                    )
                & Svg.rectWidth .~ (Svg.Px $ (fromIntegral ((columns + 1) * charWidth vd)))
                & Svg.rectHeight .~ (Svg.Px $ fromIntegral (length lines + 1) * fromIntegral (lineHeight vd))
                & Svg.drawAttr . Svg.fillColor .~ (pure Svg.FillNone)
                & strokeColour (JP.PixelRGBA8 0 0 0 255)
                & strokeWidth 2

        makePages _ [] = []
        makePages prev ((letterType, letter):xs) = 
            let group = Svg.GroupTree $ Svg.Group mempty (background : frame : letter : prev) Nothing Svg.defaultSvg
                document = Svg.Document Nothing (Just w) (Just h) [group] mempty mempty mempty mempty
            in (letterType, document) : makePages (letter:prev) xs
        in makePages [] elems

durations :: LetterType -> Double
durations StartOrMidWord = 0.04
durations EndOfWord = 0.1
durations EndOfLine = 0.5
durations EndOfFile = 2.0

highlightAndSave :: (WriteFrames :> es, BuildVideo :> es, Reader VideoData :> es) => Text -> Eff es ()
highlightAndSave text = do
    vd <- ask
    let frames = linesToSvg vd . highlightHaskell $ text
    forM_ frames $ \(letterType, frame) -> 
            addSvgDuration (durations letterType) frame 
