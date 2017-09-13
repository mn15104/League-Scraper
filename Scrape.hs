module Scrape where
import Control.Concurrent
import Control.Monad
import System.IO
import Network.Curl
import Text.HTML.Scalpel
import Text.HTML.TagSoup
import Data.Char
import Data.Tuple.Sequence
import Prelude
import Data.List
-- chroots
-- :: (Ord str, Text.StringLike.StringLike str) =>
--    Selector -> Scraper str a -> Scraper str [a]
-- The chroots function takes a selector and an inner scraper and executes 
-- the inner scraper as if it were scraping a document that consists solely of the tags 
-- corresponding to the selector. 
-- The inner scraper is executed for each set of tags matching the given selector.

-- selector
-- ("div" @: [hasClass "container"])
-- Selector defines a selection of an HTML DOM tree to be operated on by a web scraper. 
-- The selection includes the opening tag 
-- that matches the selection, all of the inner tags, and the corresponding closing tag.

-- TagString
-- Insert before x in "x" @: [hasClass ...]
 
-- text
-- text :: Selector -> Scraper str str

-- texts
-- texts:: Selector -> Scraper str [str]

-- Scraper str a
-- Monad (Scraper str)

-- scrapeURL :: URL -> Scraper str a -> IO (Maybe a)

data SummonerInfo = SummonerInfo [Ranked] [Game] deriving Show



data Game = Game Champion KDA | Zempty deriving Show
type Champion = String
type KDA = (String, String, String)
data GameResult = Victory | Defeat
data GameType = Normal | Solo | Flex

data RankedStat = RankedStat Champion KDA WinPercent Played 
type Played = Int
type WinPercent = Int
data Ranked = Ranked String String String String | Rempty deriving Show


gameHistory :: IO (Maybe [Game])
gameHistory = scrapeURL "http://euw.op.gg/summoner/userName=elliot" gameHist
    where 
        gameHist :: Scraper String [Game]
        gameHist = 
            chroots (TagString "div" @: [hasClass "GameItemWrap"])
                game
        game :: Scraper String Game
        game = do 
            let singleGame = ((TagString "div" @: [hasClass "Content"]))
            -- Returns [["k", "d", "a"], [...], ...]            
            kda' <- kda singleGame
            champ' <- champion singleGame
            return $ Game champ' kda'


rankedStats :: IO (Maybe [Ranked])
rankedStats = scrapeURL "http://euw.op.gg/summoner/userName=elliot" stats 
    where 
        stats :: Scraper String [Ranked]
        stats = chroot (TagString "div" @: [hasClass "SideContent"]) stat
        stat :: Scraper String [Ranked]
        stat = do 
            champName <- attrs "title" (TagString "div" @: [hasClass "Face"])
            kda <- texts (TagString "span" @: [hasClass "KDA"])
            let playBox = (TagString "div" @: [hasClass "Played"])
            played <- texts (playBox // (TagString "div" @: [hasClass "Title"]))
            winprc <- texts (playBox // (TagString "div" @: [hasClass "WinRatio"]))

            let playedFilt = map (filter (\c -> c /= '\n' && c /= '\t')) played
                winprcFilt = map (filter (\c -> c /= '\n' && c /= '\t')) winprc
            
            let s = zip4 champName kda playedFilt winprcFilt
                s' = map (\(a,b,c,d) -> Ranked a b c d ) s
            return s'

runScrape :: IO (SummonerInfo)
runScrape = do 
    ranked <- rankedStats 
    history <- gameHistory
    let (x,y) = case (ranked, history) of (Just x', Just y') -> (x', y')
                                          (Just x', Nothing) -> (x', [])
                                          (Nothing, Just y') -> ([], y')
                                          _ -> ([],[])

    return $ SummonerInfo x y 
    
champion :: Selector -> Scraper String Champion 
champion singleGame = do 
    let champSlc = ( singleGame //
                    (TagString "div" @: [hasClass "GameSettingInfo"]) //
                    (TagString "div" @: [hasClass "ChampionName"]))
    champName <- text champSlc
    return $ filter (\c -> c /= '\n' && c /= '\t') champName


kda :: Selector -> Scraper String KDA
kda singleGame = do
    let kda = ((TagString "div" @: [hasClass "KDA"]) //
                    (TagString "div" @: [hasClass "KDA"]))
        kdas = mapTuple3 (\slc -> (singleGame // kda // slc)) 
                    ((TagString "span" @: [hasClass "Kill"]), 
                    (TagString "span" @: [hasClass "Death"]),
                    (TagString "span" @: [hasClass "Assist"]))
    test <- sequenceT (mapTuple3 (text) kdas) 
    -- let c
    return test

mapTuple3 :: (a -> b) -> (a, a, a) -> (b, b, b)
mapTuple3 f (x,y,z) = (f x, f y, f z)

mapTuple4 :: (a -> b) -> (a, a, a, a) -> (b, b, b, b)
mapTuple4 f (s,x,y,z) = (f s, f x, f y, f z)



trim :: String -> String
-- Trimming the front is easy. Use a helper for the end.
trim = dropWhile isSpace . trim' []
  where
    trim' :: String -> String -> String
    -- When finding whitespace, put it in the space bin. When finding
    -- non-whitespace, include the binned whitespace and continue with an
    -- empty bin. When at the end, just throw away the bin.
    trim' _ [] = []
    trim' bin (a:as) | isSpace a = trim' (bin ++ [a]) as
                     | otherwise = bin ++ a : trim' [] as