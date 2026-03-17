module GenerateAudio (
    setTidalPattern, 
    runBuildAudio
) where

import VideoProps


import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Reader.Static
import qualified Sound.Tidal.Boot as Tidal
import qualified Sound.Tidal.Context as Tidal
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE tidalInst #-}
tidalInst = unsafePerformIO Tidal.mkTidal

instance Tidal.Tidally where tidal = tidalInst


data BuildAudio :: Effect where
    SetTidalPattern :: Tidal.ControlPattern -> BuildAudio m ()

type instance DispatchOf BuildAudio = Dynamic

setTidalPattern :: (BuildAudio :> es) => Tidal.ControlPattern -> Eff es ()
setTidalPattern = send . SetTidalPattern

runBuildAudio :: (IOE :> es, Reader VideoProps :> es) => Eff (BuildAudio : es) a -> Eff (es) a
runBuildAudio eff = do
    res <- interpretWith eff $ \_ -> \case 
        SetTidalPattern controlPattern -> do
            liftIO $ Tidal.d1 controlPattern

    return res
