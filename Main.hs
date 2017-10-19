{-# LANGUAGE Arrows       #-}
{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

-- dunai
import Control.Monad.Trans.MSF.Except

-- rhine
import FRP.Rhine
import FRP.Rhine.Clock.Realtime.Millisecond


-- TODO Need to generalise the IO clocks to MonadIO
main :: IO ()
main = flow mainRhine


data Tea = Tea
  { teaSort  :: String -- ^ The sort, brand, type of tea
  , duration :: Double -- ^ The duration to brew in minutes
  }

-- TODO Also record intermediate step where we just output a string instead of throwing an exception
countdownTea :: Tea -> Behaviour (ExceptT String m) Double ()
countdownTea Tea {} = proc _ -> do
  timeSoFar <- integral        -< 1 / 60 -- TODO Can we do something better like measuring the time on start
  _         <- timeless (throwOn teaSort) -< timeSoFar >= duration
  returnA                      -< ()

testTea = Tea
  { teaSort  = "Earl Grey"
  , duration = 3
  }

-- TODO To make this work in a readable way, dunai should really switch to Arrow Transformer classes
teaLoop = safely $ do
  teaSort <- try $ countdownTea testTea
  _ <- once_ $ putStrLn $ "Your " ++ teaSort ++ " is ready!"
  safe teaLoop -- TODO Alternatively do the safely outside

mainRhine :: Rhine IO (Millisecond 50) () ()
mainRhine = teaLoop @@ waitClock
