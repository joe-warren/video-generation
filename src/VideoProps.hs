module VideoProps where

import Data.Default
import Control.Lens

data VideoProps = VideoProps
    { _videoWidth :: Integer
    , _videoHeight :: Integer
    , _videoFPS :: Integer
    , _videoScratchDir :: FilePath
    , _videoOutputFile :: FilePath
    } deriving (Show, Eq)

makeLenses ''VideoProps


instance Default VideoProps where
    def = VideoProps 
        { _videoWidth = 1920
        , _videoHeight = 1080
        , _videoFPS = 25
        , _videoScratchDir = "output"
        , _videoOutputFile = "output.mp4"
        } 