module Util where

import Control.Monad.Fail-- dunai
import Control.Monad.Trans.MSF.Except
import Data.MonadicStreamFunction

-- | Accumulates inputs and starts an MSF for each of them
pool :: MonadFail m => (a -> MSF m () b) -> MSF m [a] [b]
pool f = pool' f []
  where
    pool' :: Monad m => (a -> MSF m () b) -> [MSF m () b] -> MSF m [a] [b]
    pool' f msfs = MSF $ \as -> do
      let moremsfs = msfs ++ map f as
      (bs, msfs') <- unzip <$> sequence (map (flip unMSF ()) moremsfs)
      return (bs, pool' f msfs')

-- | Remembers and indefinitely outputs the first input value.
keepFirst :: MonadFail m => MSF m a a
keepFirst = safely $ do
  a <- try throwS
  safe $ arr $ const a
-- TODO This would somewhere in dunai, but putting it in Data.MonadicStreamFunction.Util forms an import cycle
-- => break the cycle by making all the Trans.MSF modules depend on Core only?
