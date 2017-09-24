import Control.Concurrent
import Control.Monad
import System.IO
import Summoner
import LiveGame
import Champion


-- listFork :: IO()
-- listFork = 

mapForkIO :: (a -> IO b) -> [a] -> IO [b]
mapForkIO f xs = do 
  mvars <- replicateM (length xs) newEmptyMVar 
  ls <- sequence $ zipWith (\x mv -> f x >>= \x' -> forkIO $ putMVar mv x' ) (xs) mvars  
  mvars' <- traverse takeMVar mvars
  return mvars'

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering   
  let summoners = ["Elliot", "omgdiamonds"]
  ls' <- mapForkIO runSummoner summoners
  traverse (putStrLn . show) ls'
  return ()

