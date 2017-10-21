{-# LANGUAGE Arrows         #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies   #-}


-- rhine
import FRP.Rhine
import FRP.Rhine.SyncSF.Except
import FRP.Rhine.Clock.Realtime.Millisecond


type TeaSimClock = Millisecond 100
type TeaStatusClock = Millisecond 10000


-- TODO Need to generalise the IO clocks to MonadIO
main :: IO ()
main = flow mainRhine


data Tea = Tea
  { teaSort  :: String -- ^ The sort, brand, type of tea
  , duration :: Double -- ^ The duration to brew in minutes
  }

-- TODO Also record intermediate step where we just output a string instead of throwing an exception
countdownTea
  :: Monad m
  => Tea -> Behaviour (ExceptT String m) UTCTime ()
countdownTea Tea {..} = proc _ -> do
  timeSoFar <- integral        -< 1 / 60 -- TODO Can we do something better like measuring the time on start
  _         <- throwOn teaSort -< timeSoFar >= duration
  returnA                      -< ()

testTea = Tea
  { teaSort  = "Earl Grey"
  , duration = 0.1
  }

-- TODO To make this work in a readable way, dunai should really switch to Arrow Transformer classes
teaLoop :: SyncExcept IO TeaSimClock () () Empty
teaLoop = do
  teaSort <- try $ countdownTea testTea
  _ <- once_ $ putStrLn $ "Your " ++ teaSort ++ " is ready!"
  teaLoop

mainRhine :: Rhine IO TeaSimClock () ()
mainRhine = safely teaLoop @@ waitClock
