module CodeBlock where

import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Error.Static
import Effectful.Exception
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.Map (Map)
import qualified Data.Map as M
import Control.Arrow (first)

data CodeBlockError = 
    CodeBlockFileNotFound FilePath
    | CodeBlockBlockNotFound FilePath Text
    deriving Show

data LoadCodeBlocks :: Effect where
    LoadCodeBlock :: FilePath -> Text -> LoadCodeBlocks m Text

type instance DispatchOf LoadCodeBlocks = Dynamic

loadCodeBlock :: (LoadCodeBlocks :> es) => FilePath -> Text -> Eff es Text
loadCodeBlock filePath blockName = send $ LoadCodeBlock filePath blockName

blockPrefix :: Text
blockPrefix = "-- BLOCK:"

parseCodeBlocks :: [Text] -> Map Text [Text]
parseCodeBlocks = 
    let go [] = ([], M.empty)
        go (l:xs) = 
            case T.stripPrefix blockPrefix l of 
                Just l' -> 
                    let (thisBlock, rest) = go xs
                    in ([] , M.singleton (T.strip l') thisBlock <> rest)
                Nothing -> first (l:) $ go xs
        isBlank = T.all (isSpace)
        trimBlankLines = dropWhile isBlank . dropWhileEnd isBlank
    in fmap trimBlankLines . snd <$> go


runLoadCodeBlocks ::
    ( IOE :> es
    , Error CodeBlockError :> es ) =>
    Eff (LoadCodeBlocks : es) a -> Eff es a
runLoadCodeBlocks = 
    interpret $ \_ -> \case 
        LoadCodeBlock fp blockName -> do
            fileContents <- (liftIO $ T.readFile fp) `catchIO` \_ -> throwError (CodeBlockFileNotFound fp)
            let blockMap = parseCodeBlocks . T.lines $ fileContents
            case M.lookup blockName blockMap of
                Just block -> do 
                    liftIO $ T.putStrLn blockName
                    liftIO $ traverse T.putStrLn block
                    return . T.unlines $ block
                Nothing -> throwError $ CodeBlockBlockNotFound fp blockName

logCodeBlockError :: (IOE :> es) => Eff (Error CodeBlockError : es) () -> Eff es () 
logCodeBlockError = 
    let handler callstack e = liftIO $ do
            putStrLn "CodeBlock Error"
            print e
            print callstack
    in runErrorWith handler