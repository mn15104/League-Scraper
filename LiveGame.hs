{-# LANGUAGE Arrows, OverloadedStrings  #-}

module LiveGame where
import Control.Concurrent
import Control.Monad
import Control.Arrow
import Control.Category hiding ((.))
import System.IO
import Network.Curl
import Text.HTML.Scalpel
import Text.HTML.TagSoup
import Data.Char
import Data.Tuple.Sequence
import Prelude hiding (id)
import Data.List

data Team = Team {blueTeam :: [(Summoner, Champion)], redTeam :: [(Summoner, Champion)]} deriving Show
type Summoner = String
type Champion = String

findPlayers :: IO (Maybe Team)
findPlayers = scrapeURL ("http://www.lolskill.net/game/LA2/Snepif") stats 
    where 
        stats :: Scraper String Team
        stats = do 
             summoner <- texts ("p" @: [hasClass  "summoner-name"])  
             champion <- attrs "style" ("div" @: [hasClass "gamesummoner-card"])
             let champion' = map (parseChampion . filterSpecials) champion
                 summoner' = removeDuplicates summoner
                 team = Team ((take 5) $ zip summoner' champion')
                             ((drop 5) $ zip summoner' champion')
             return team
        players :: Scraper String String 
        players = text ("td" @: [hasClass "name"])


parseChampion :: String -> String 
parseChampion s = reverse . takeWhile (/='/') . drop 1 . dropWhile (/= '_') . reverse $ s

filterSpecials :: String -> String 
filterSpecials s = (filter (\c -> c /= '\n' && c /= '\t')) s

runLiveGame :: IO Team
runLiveGame = do 
    s <- findPlayers 
    putStrLn $ show s
    return $ case s of  Just team -> team
                        Nothing -> Team [] []

removeDuplicates :: (Eq a) => [a] -> [a]   
removeDuplicates = foldl (\seen x -> if x `elem` seen
    then seen
    else seen ++ [x]) []