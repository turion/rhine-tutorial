{-# LANGUAGE DataKinds #-}

-- base
import Control.Concurrent (threadDelay)
import System.IO (hFlush, stdout)

-- dunai
import Control.Monad.Trans.MSF.Maybe (runMaybeT, MaybeT, exit)

-- rhine
import FRP.Rhine
import FRP.Rhine.Clock.Realtime.Millisecond


-- TODO Need to generalise the IO clocks to MonadIO
main :: IO ()
main = do
  _ <- runMaybeT $ flow mainRhine
  return ()

mainRhine :: Rhine (MaybeT IO) (LiftClock IO MaybeT (Millisecond 50)) () ()
mainRhine =   timeless (listToMaybeS "Congratulations! You've installed the tutorial correctly!\n")
          >-> liftS (putChar >>> (>> hFlush stdout))
          @@  liftClock waitClock


-- TODO In dunai 0.1.2
listToMaybeS :: Monad m => [b] -> MSF (MaybeT m) a b
listToMaybeS = foldr iPost exit
