module VideoData
( VideoData (..)
) where

data VideoData = VideoData 
    { videoWidth :: Integer
    , videoHeight :: Integer
    , scratchDir :: FilePath
    } 