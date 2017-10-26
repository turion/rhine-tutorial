{-# LANGUAGE Arrows         #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies   #-}


-- base
import Data.Either (rights)
import Text.Read (readMaybe)

-- rhine
import FRP.Rhine
import FRP.Rhine.SyncSF.Except
import FRP.Rhine.Clock.Realtime.Millisecond
import FRP.Rhine.Clock.Realtime.Stdin
import FRP.Rhine.Clock.Select
import FRP.Rhine.Schedule.Concurrently
import FRP.Rhine.ResamplingBuffer.KeepLast
import FRP.Rhine.ResamplingBuffer.Collect

-- rhine-tutorial
import Util

-- TODO add all imports to master?

data Tea = Tea
  { teaSort  :: String -- ^ The sort, brand, type of tea
  , duration :: Double -- ^ The duration to brew in minutes
  }
  deriving (Read, Show)


type TeaSimClock = Millisecond 100
type TeaStatusClock = Millisecond 20000

type CommandClock = SelectClock StdinClock Tea
commandClock :: CommandClock
commandClock = SelectClock
  { mainClock = StdinClock
  , select    = readMaybe
  }

userTeas :: SyncSF IO CommandClock () Tea
userTeas = proc _ -> do
  tea <- timeInfoOf tag  -< ()
  _   <- arrMSync putStrLn -< "Your request: " ++ show tea
  returnA                -< tea


-- TODO Also record intermediate step where we just output a string instead of throwing an exception
countdownTea
  :: Monad m
  => Tea -> SyncSF (ExceptT String m) TeaSimClock a ()
countdownTea Tea {..} = proc _ -> do
  now   <- timeInfoOf absolute -< ()
  start <- keepFirst           -< now
  let
    minutesPassed = (now `diffTime` start) / 60
    done          = (minutesPassed >= duration, teaSort)
  _     <- throwOn'            -< done
  returnA                      -< ()

exampleTea = Tea
  { teaSort  = "English Breakfast Tea"
  , duration = 0.2
  }

oneTea :: Tea -> SyncExcept IO TeaSimClock a () ()
oneTea nextTea = do
  -- nextTea <- currentInput
  once_ $ putStrLn $ "Now brewing: " ++ show nextTea
  teaSort <- try $ countdownTea nextTea
  once_ $ putStrLn $ "Your " ++ teaSort ++ " is ready!"
  step $ const $ return ((), ())

teas :: SyncSF IO TeaSimClock [Tea] [Either () ()]
teas = pool $ exceptS . runMSFExcept . oneTea

teaStatus :: SyncSF IO TeaStatusClock [Either () ()] ()
teaStatus = proc as -> do
  let numberOfTeas = length $ rights as
  arrMSync putStrLn -< show numberOfTeas ++ " teas currently brewing"

mainRhine
  =   userTeas    @@  commandClock
  >-- collect     -@- concurrently
  --> teas        @@  waitClock
  >-- keepLast [] -@- concurrently
  --> teaStatus   @@  waitClock

main :: IO ()
main = flow mainRhine
