module VideoData
( VideoData (..)
) where

data VideoData = VideoData 
    { videoWidth :: Integer
    , videoHeight :: Integer
    , videoFPS :: Integer
    , scratchDir :: FilePath
    , videoOutputFile :: FilePath
    } 