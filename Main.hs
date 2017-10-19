-- dunai
import Control.Monad.Trans.MSF.Maybe

-- rhine
import FRP.Rhine.Clock.Count -- TODO Try
import FRP.Rhine.Clock.Realtime.Busy
import FRP.Rhine.Clock.Realtime.Millisecond
import FRP.Rhine.ResamplingBuffer.FIFO

-- TODO Need to generalise the IO clocks to MonadIO
main = runMaybeT $ flow $ listToMSF "Congratulations! You've installed the tutorial correctly!" >-> arrM putChar @@ (waitClock :: Millisecond 100)
