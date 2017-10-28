{-# LANGUAGE Arrows                #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

-- dunai
import Data.MonadicStreamFunction
import Data.VectorSpace.Specific()

-- rhine
import FRP.Rhine
import FRP.Rhine.SyncSF.Except
import FRP.Rhine.Clock.Realtime.Millisecond

---------------------------

verboseSum :: MSF IO Int Int
verboseSum = proc n -> do
  s <- sumS       -< n
  _ <- arrM print -< "The sum is now " ++ show s
  returnA         -< s


main1 = reactimate
  $   arrM_ (putStrLn "Enter a number:" >> readLn)
  >>> verboseSum
  >>> arr (const ())

---------------------------

type SumClock = Millisecond 100

fillUp :: Monad m => SyncSF (ExceptT Double m) SumClock Double ()
fillUp = proc x -> do
  s <- integral -< x
  _ <- throwOn' -< (s > 5, s)
  returnA       -< ()

helloWorld :: SyncExcept IO SumClock () () Empty
helloWorld = do
  try $ arr (const 1) >>> fillUp
  once_ $ putStrLn "Hello World!"
  helloWorld

main = flow $ safely helloWorld @@ waitClock

---------------------------

data FastClock = FastClock
instance Clock m FastClock where
  type TimeDomainOf FastClock = ()
  type Tag          FastClock = ()

data SlowClock = SlowClock
instance Clock m SlowClock where
  type TimeDomainOf SlowClock = ()
  type Tag          SlowClock = ()

fastSignal :: SyncSF m FastClock () a
fastSignal = undefined

slowProcessor :: SyncSF m SlowClock b c
slowProcessor = undefined

-- uncomment the following for a clock type error
-- clockTypeError = fastSignal >>> slowProcessor

---------------------------

-- rhmain = putStrLn "Uncomment one of the example mains in the file PresentationExamples.hs!"
