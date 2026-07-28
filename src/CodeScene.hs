module CodeScene where

import VideoProps
import SvgUtils
import GenerateVideo (BuildVideo, addSvgDuration)

import Data.Text (Text)
import Data.List (intersperse, intercalate, inits, tails)
import qualified Data.Text as T
import qualified Skylighting as Sky
import qualified Graphics.Svg as Svg
import qualified Codec.Picture.Types as JP
import Control.Lens
import Control.Monad (join, forM_)
import Data.Monoid (Last (..))
import Control.Applicative((<|>))
import Control.Arrow (second)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Effectful
import Effectful.Reader.Static
import Effectful.Error.Static
import Data.Default
import Data.Algorithm.Diff (Diff, PolyDiff (..), getGroupedDiffBy)

data Alignment = Top | Middle | Bottom 
    deriving Show

data CodeSceneProps = CodeSceneProps
    { _codeSceneFileExtension :: Text
    , _codeSceneColumns :: Integer
    , _codeSceneBorderColumns :: Integer
    , _codeSceneFrameWidth :: Double
    , _codeSceneStyle :: Sky.Style
    , _codeSceneAlignment :: Alignment
    } deriving (Show)

instance Default CodeSceneProps where
    def = CodeSceneProps 
        { _codeSceneFileExtension = "hs"
        , _codeSceneColumns = 80
        , _codeSceneBorderColumns = 2 
        , _codeSceneFrameWidth = 2.0
        , _codeSceneStyle = Sky.haddock
        , _codeSceneAlignment = Middle
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
    | HighlightErrorEmptyHighlight
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
    let sceneStyle = cs ^. codeSceneStyle
        tokStyle = tokType `M.lookup` Sky.tokenStyles sceneStyle
        tokCol = fromMaybe (Sky.RGB 0 0 0) ((Sky.tokenColor =<< tokStyle) <|> Sky.defaultColor sceneStyle) 
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
        yOff = fromIntegral $ case cs ^. codeSceneAlignment of 
            Top -> lineHeight vd cs
            Middle -> (vd ^. videoHeight - fromIntegral (length lines) * lineHeight vd cs) `div` 2
            Bottom -> vd ^. videoHeight - fromIntegral (length lines + 2) * lineHeight vd cs

        transform (i, elems) =  second (translate xOff (yOff + fromIntegral (lineHeight vd cs) * i)) <$> elems
        elems = lines
            & fmap (lineToSvg vd cs)
            & zip [1..]
            & fmap transform
            & join
        backgroundColor = fromMaybe white $ convertColor <$> Sky.backgroundColor (cs ^. codeSceneStyle)
        background = Svg.RectangleTree $ 
            Svg.defaultSvg 
                & Svg.rectUpperLeftCorner .~ (Svg.Px 0, Svg.Px 0)
                & Svg.rectWidth .~ w
                & Svg.rectHeight .~ h
                & colour backgroundColor
                
        frameColor = fromMaybe black $ convertColor <$> Sky.defaultColor (cs ^. codeSceneStyle)

        frame = Svg.RectangleTree $ 
            Svg.defaultSvg 
                & Svg.rectUpperLeftCorner .~ 
                    ( Svg.Px $ xDelta + (fromIntegral (cs ^. codeSceneBorderColumns) - 0.5) * fromIntegral (charWidth vd cs)
                    , Svg.Px yOff
                    )
                & Svg.rectWidth .~ (Svg.Px $ (fromIntegral ((cs ^. codeSceneColumns + 1) * charWidth vd cs)))
                & Svg.rectHeight .~ (Svg.Px $ fromIntegral (length lines + 1) * fromIntegral (lineHeight vd cs))
                & Svg.drawAttr . Svg.fillColor .~ (pure Svg.FillNone)
                & strokeColour frameColor
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
    ( BuildVideo :> es
    , Reader VideoProps :> es
    , Error HighlightError :> es
    ) => CodeSceneProps -> Double -> Text -> Eff es Svg.Document
codeScene cs duration text = do
    vd <- ask
    frames <- linesToSvg vd cs <$> highlight (cs ^. codeSceneFileExtension) text

    case unsnoc frames of 
        Nothing -> throwError HighlightErrorEmptyHighlight
        Just (transitionFrames, lastFrame) -> do
            let normalizedLength = sum (durationWeight . fst <$> transitionFrames)
            forM_ transitionFrames $ \(letterType, frame) -> 
                let dur =
                        duration 
                            * durationWeight letterType
                            / normalizedLength
                in addSvgDuration dur frame 
            return . snd $ lastFrame

        
data DiffTok = Newline | DiffTok Sky.Token

compareDiffToc :: DiffTok -> DiffTok -> Bool
compareDiffToc Newline Newline = True
compareDiffToc (DiffTok (_, textA)) (DiffTok (_, textB)) = textA == textB
compareDiffToc _ _ = False

linesToDiffToks :: [[Sky.Token]] -> [DiffTok]
linesToDiffToks = intercalate [Newline] . fmap (fmap DiffTok)

diffToksToLines :: [DiffTok] -> [[Sky.Token]]
diffToksToLines = 
    let step Newline ls = [] : ls
        step (DiffTok tok) (l:ls) = (tok:l) : ls
        step (DiffTok tok) [] = [[tok]]
    in foldr step [[]]

diffToSequence :: [Diff [DiffTok]] -> [[DiffTok]]
diffToSequence diff = 
    let prefixes x = 
            let go (DiffTok (tokType, t)) = (DiffTok . (tokType, ) <$> drop 1 (T.inits t))
                go Newline = [Newline]
            in do
                prefix <- drop 1 $ inits x 
                traverseOf _last go prefix
        during (Both x _) = pure x
        during (Second x) = prefixes x
        during (First x) = reverse . prefixes $ x
        before (Both x _) = x
        before (First x) = x
        before (Second x) = []
        after (Both x _) = x
        after (First x) = []
        after (Second x) = x
    in do
        (done, focus : todo) <- zip (inits diff) (tails diff)
        mid <- during focus
        pure $ foldMap after done <> mid <> foldMap before todo

diffScene :: 
    ( BuildVideo :> es
    , Reader VideoProps :> es
    , Error HighlightError :> es
    ) => CodeSceneProps -> Double -> Text -> Text -> Eff es Svg.Document
diffScene cs duration textBefore textAfter = do
    let highlight' t = linesToDiffToks <$> highlight (cs ^. codeSceneFileExtension) t
    highlightBefore <- highlight' textBefore
    highlightAfter <- highlight' textAfter

    vd <- ask

    let diff = getGroupedDiffBy compareDiffToc highlightBefore highlightAfter
        transitionFrames =
            snd 
            . last
            . linesToSvg vd cs 
            . diffToksToLines 
            <$> diffToSequence diff
        len = length transitionFrames
        frameDur = duration / fromIntegral len
 
    forM_ transitionFrames (addSvgDuration frameDur)
    
    return . last $ transitionFrames