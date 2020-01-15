{-# LANGUAGE Arrows          #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}
{-# LANGUAGE TypeFamilies    #-}


-- base
import Control.Concurrent (threadDelay)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

-- dunai
import Control.Monad.Trans.MSF.Maybe (runMaybeT, MaybeT (MaybeT), exit)
import qualified Control.Monad.Trans.MSF.Except as Dunai

-- rhine
import FRP.Rhine
import FRP.Rhine.Clock.Realtime.Millisecond
import FRP.Rhine.Clock.Realtime.Stdin
import FRP.Rhine.Clock.Select
import FRP.Rhine.Schedule.Concurrently
import FRP.Rhine.ResamplingBuffer.KeepLast
import FRP.Rhine.ResamplingBuffer.Collect

-- rhine-tutorial
import Util


main :: IO ()
main = do
  _ <- runMaybeT $ flow mainRhine
  return ()

mainRhine :: Rhine (MaybeT IO) (LiftClock IO MaybeT (Millisecond 50)) () ()
mainRhine =   timeless (listToMaybeS "Congratulations! You've installed the tutorial correctly!\n")
          >-> arrMCl oneCharacter
          @@  liftClock waitClock

oneCharacter :: Char -> MaybeT IO ()
oneCharacter c = liftIO $ do
  putChar c
  hFlush stdout

-- bug #170 in dunai-0.5.1

exceptToMaybeS :: Monad m => MSF (ExceptT e m) a b -> MSF (MaybeT m) a b
exceptToMaybeS = morphS $ MaybeT . fmap (either (const Nothing) Just) . runExceptT

listToMaybeS :: Monad m => [a] -> MSF (MaybeT m) arbitrary a
listToMaybeS = exceptToMaybeS . runMSFExcept . sequence . map (Dunai.step . const . return . (, ()))
