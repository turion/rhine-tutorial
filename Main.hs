{-# LANGUAGE Arrows          #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies    #-}


-- base
import Control.Concurrent (threadDelay)
import Control.Monad.Fail
import Data.Either (rights)
import System.IO (hFlush, stdout)
import Text.Read (readMaybe)

-- dunai
import Control.Monad.Trans.MSF.Maybe (runMaybeT, MaybeT, exit)


-- rhine
import FRP.Rhine
import FRP.Rhine.ClSF.Except
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
          >-> liftS (putChar >>> (>> hFlush stdout))
          @@  liftClock waitClock


-- TODO In dunai 0.1.2
listToMaybeS :: MonadFail m => [b] -> MSF (MaybeT m) a b
listToMaybeS = foldr iPost exit
