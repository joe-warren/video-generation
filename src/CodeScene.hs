module CodeScene where

import VideoProps
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
import Effectful.Error.Static
import Data.Default

data CodeSceneProps = CodeSceneProps
    { _codeSceneFileExtension :: Text
    , _codeSceneColumns :: Integer
    , _codeSceneBorderColumns :: Integer
    , _codeSceneFrameWidth :: Double
    , _codeSceneStyle :: Sky.Style
    , _codeSceneTransitionDuration :: Double 
    , _codeSceneStillDuration :: Double
    } deriving (Show)

instance Default CodeSceneProps where
    def = CodeSceneProps 
        { _codeSceneFileExtension = "hs"
        , _codeSceneColumns = 80
        , _codeSceneBorderColumns = 2 
        , _codeSceneFrameWidth = 2.0
        , _codeSceneStyle = Sky.haddock
        , _codeSceneTransitionDuration = 2.0
        , _codeSceneStillDuration = 1.0
        }

makeLenses ''CodeSceneProps

charWidth :: VideoProps -> CodeSceneProps -> Integer
charWidth vd cs = vd ^. videoWidth `div` (cs ^. codeSceneColumns + 2 * cs ^. codeSceneBorderColumns)

charHeight :: VideoProps -> CodeSceneProps -> Integer
charHeight vd cs = charWidth vd cs * 2

lineHeight :: VideoProps -> CodeSceneProps -> Integer 
lineHeight vd cs = (charWidth vd cs * 5) `div` 2

data HighlightError = 
    HighlightErrorMissingSyntax Text
    | HighlightErrorAmbiguousSyntax Text
    | HighlightErrorInTokenizer Text
    deriving Show

runHighlight :: (IOE :> es) => Eff (Error HighlightError : es) () -> Eff es () 
runHighlight = 
    let handler callstack e = liftIO $ do
            putStrLn "Highlighting Error"
            print e
            print callstack
    in runErrorWith handler

highlight :: (Error HighlightError :> es) => Text -> Text -> Eff es [Sky.SourceLine]
highlight extension text = do
    let tokenizerConfig = Sky.TokenizerConfig Sky.defaultSyntaxMap False
    syntax <- 
        case Sky.syntaxesByExtension Sky.defaultSyntaxMap (T.unpack extension) of    
            [s] -> pure s
            [] -> throwError (HighlightErrorMissingSyntax extension)
            _ -> throwError (HighlightErrorAmbiguousSyntax extension)
    let wrapTokenizerError = either (throwError . HighlightErrorInTokenizer . T.pack) pure
    wrapTokenizerError $ Sky.tokenize tokenizerConfig syntax text


convertColor :: Sky.Color -> JP.PixelRGBA8
convertColor (Sky.RGB r g b) = JP.PixelRGBA8 r g b 255

tokenColour :: CodeSceneProps -> Sky.TokenType -> JP.PixelRGBA8
tokenColour cs tokType =
    let tokStyle = tokType `M.lookup` Sky.tokenStyles (cs ^. codeSceneStyle)
        tokCol = fromMaybe (Sky.RGB 0 0 0) (Sky.tokenColor =<< tokStyle)
    in convertColor tokCol

data LetterType = StartOrMidWord | EndOfWord | EndOfLine

lineToSvg :: VideoProps -> CodeSceneProps -> Sky.SourceLine -> [(LetterType, Svg.Tree)]
lineToSvg vd cs = 
    let f _ [] = []
        f offset ((_, ' '):xs) = f (offset+1) xs
        f offset ((tokenType, t):xs) =
            let trans = translate (fromIntegral (charWidth vd cs) * offset) 0
                font a = a 
                    & Svg.drawAttr . Svg.fontFamily .~ (pure ["Share Tech Mono"])
                    & Svg.drawAttr . Svg.fontSize .~ (pure . Svg.Px . fromIntegral $ charHeight vd cs)
                col = colour (tokenColour cs tokenType)
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

linesToSvg :: VideoProps -> CodeSceneProps -> [Sky.SourceLine] -> [(LetterType, Svg.Document)]
linesToSvg vd cs lines = 
    let w = Svg.Num . fromIntegral $ vd ^. videoWidth
        h = Svg.Num .fromIntegral $ vd ^. videoHeight

        xDelta = fromIntegral $ (vd ^. videoWidth - (charWidth vd cs * ((cs ^. codeSceneColumns) + 2 * cs ^. codeSceneBorderColumns))) `div` 2
        xOff = xDelta + (fromIntegral $ charWidth vd cs * cs ^. codeSceneBorderColumns)
        yOff = fromIntegral ((vd ^. videoHeight - fromIntegral (length lines) * lineHeight vd cs) `div` 2)

        transform (i, elems) =  second (translate xOff (yOff + fromIntegral (lineHeight vd cs) * i)) <$> elems
        elems = lines
            & fmap (lineToSvg vd cs)
            & zip [1..]
            & fmap transform
            & join
        background = Svg.RectangleTree $ 
            Svg.defaultSvg 
                & Svg.rectUpperLeftCorner .~ (Svg.Px 0, Svg.Px 0)
                & Svg.rectWidth .~ w
                & Svg.rectHeight .~ h
                & colour (JP.PixelRGBA8 255 255 255 255)

        frame = Svg.RectangleTree $ 
            Svg.defaultSvg 
                & Svg.rectUpperLeftCorner .~ 
                    ( Svg.Px $ xDelta + (fromIntegral (cs ^. codeSceneBorderColumns) - 0.5) * fromIntegral (charWidth vd cs)
                    , Svg.Px . fromInteger $ ((vd ^. videoHeight - fromIntegral (length lines) * lineHeight vd cs) `div` 2)
                    )
                & Svg.rectWidth .~ (Svg.Px $ (fromIntegral ((cs ^. codeSceneColumns + 1) * charWidth vd cs)))
                & Svg.rectHeight .~ (Svg.Px $ fromIntegral (length lines + 1) * fromIntegral (lineHeight vd cs))
                & Svg.drawAttr . Svg.fillColor .~ (pure Svg.FillNone)
                & strokeColour (JP.PixelRGBA8 0 0 0 255)
                & strokeWidth 2

        makePages _ [] = []
        makePages prev ((letterType, letter):xs) = 
            let group = Svg.GroupTree $ Svg.Group mempty (background : frame : letter : prev) Nothing Svg.defaultSvg
                document = Svg.Document Nothing (Just w) (Just h) [group] mempty mempty mempty mempty
            in (letterType, document) : makePages (letter:prev) xs
        in makePages [] elems

durationWeight :: LetterType -> Double
durationWeight StartOrMidWord = 0.8
durationWeight EndOfWord = 0.2
durationWeight EndOfLine = 1.0

codeScene :: 
    ( WriteFrames :> es
    , BuildVideo :> es
    , Reader VideoProps :> es
    , Error HighlightError :> es
    ) => CodeSceneProps -> Text -> Eff es ()
codeScene cs text = do
    vd <- ask
    frames <- linesToSvg vd cs <$> highlight (cs ^. codeSceneFileExtension) text

    case unsnoc frames of 
        Nothing -> pure ()
        Just (transitionFrames, lastFrame) -> do
            let normalizedLength = sum (durationWeight . fst <$> transitionFrames)
            forM_ transitionFrames $ \(letterType, frame) -> 
                let dur =
                        cs ^. codeSceneTransitionDuration 
                            * durationWeight letterType
                            / normalizedLength
                in addSvgDuration dur frame 
            addSvgDuration (cs ^. codeSceneStillDuration) (snd lastFrame)
