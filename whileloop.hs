import           Control.Monad
import           Control.Monad.Trans
import           Control.Monad.Trans.Maybe
import           Data.IORef
import           Text.Printf

while :: (Monad m) => m Bool -> m a -> m ()
while cond_ stat = do
  cond <- cond_
  when cond $ do
    _ <- stat
    while cond_ stat

loop :: Monad m => MaybeT m a -> m ()
loop stat = (runMaybeT $ forever stat) >> return ()

main :: IO ()
main = do
  i <- newIORef (1::Int)
  loop $ do
    j <- lift $ readIORef i
    lift $ printf "%d\n" j
    lift $ modifyIORef' i (+1)
    when (j == 100) mzero
  return ()
