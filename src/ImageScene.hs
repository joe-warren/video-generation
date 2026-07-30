module ImageScene
( addImageDuration
, loadImage
, runLoadImages
) where 

import Effectful
import Effectful.Dispatch.Dynamic

import qualified Data.ByteString as BS
import Data.ByteString.Base64 (encodeBase64)
import Data.Base64.Types (extractBase64)
import Data.Char (toLower)
import qualified Data.Text as T
import SvgUtils (imageFile)
import GenerateVideo (addSvgDuration, BuildVideo)
import Effectful.Reader.Static
import VideoProps
import qualified Graphics.Svg as Svg
import System.FilePath (takeExtension)


data LoadImages :: Effect where
    LoadImage :: FilePath -> LoadImages m Svg.Document

    
type instance DispatchOf LoadImages = Dynamic

loadImage :: (LoadImages :> es) => FilePath -> Eff es Svg.Document
loadImage = send . LoadImage 

runLoadImages :: (IOE :> es, Reader VideoProps :> es) => Eff (LoadImages : es) a -> Eff (es) a
runLoadImages = interpret $ \_ -> \case 
    LoadImage filepath -> do
        vd <- ask
        contents <- liftIO $ BS.readFile filepath
        let replaceJpg "jpg" = "jpeg"
            replaceJpg x = x 
            mimeSuffix = replaceJpg . fmap toLower . drop 1 . takeExtension $ filepath
            header = "data:image/" <> mimeSuffix <> ";base64,"
        return $ imageFile vd (header <> T.unpack (extractBase64 (encodeBase64 contents)))

addImageDuration :: (BuildVideo :> es, LoadImages :> es) => Double -> FilePath -> Eff es ()
addImageDuration duration fp = addSvgDuration duration =<< loadImage fp