module ConcatFileVideo
( generateVideo 
) where

import Numeric (showFFloat)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified System.Process as Process
 

writeConcatFile :: FilePath -> [(Int, FilePath)] -> IO ()
writeConcatFile filepath= 
    let showFloat f = T.pack $ showFFloat Nothing f [] 
        go (nFrames, path) = replicate nFrames
            ("file '" <> T.pack path <> "'\n" <>
            "duration " <> showFloat 0.04 <> "\n")
    in T.writeFile filepath . T.concat . (go =<<)

generateVideo :: FilePath -> FilePath -> [(Int, FilePath)] -> IO ()
generateVideo scratchDir outputFile frames = do
    let concatFileName = "concat.txt"
    writeConcatFile concatFileName frames
    Process.callProcess "ffmpeg"
        [ "-y"
        , "-f", "concat"
        , "-i", concatFileName
        , "-vsync", "vfr"
        , "-pix_fmt", "yuv420p"
        , outputFile
        ]

    return ()