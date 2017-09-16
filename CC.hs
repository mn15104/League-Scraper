import Control.Concurrent
import Control.Monad
import System.IO
import Summoner

main = do
  hSetBuffering stdout NoBuffering            -- 1
  s <- newEmptyMVar 
  forkIO $ putMVar s runScrape  -- 2
  (SummonerInfo m m') <- join $ takeMVar s    -- SummonerInfo [Ranked] [Game]
--   let s = map (map . putStrLn . show) m
  return ""