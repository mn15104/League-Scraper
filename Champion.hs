{-# LANGUAGE Arrows, OverloadedStrings  #-}

module Champion where
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

data Role = Top | Mid | Jungle | ADC | Support


championStats :: String -> IO (Maybe String)
championStats champ = scrapeURL ("http://na.op.gg/champion/" ++ champ ++"/statistics") stats 
    where 
        stats :: Scraper String String 
        stats = do 
            label <- text ( "div" @: [ hasClass "Stats"])
            let winRate = parseBrackets . filterSpecials $ label
            return winRate
            -- return $ case label of "Win Rate"
            -- s <- text select 
            -- return s

parseBrackets :: String -> String 
parseBrackets s = (takeWhile (/= ')')) . (drop 1) . (dropWhile (/= '('))  $ s           

filterSpecials :: String -> String 
filterSpecials s = (filter (\c -> c /= '\n' && c /= '\t')) s

run :: IO ()
run = do 
    s <- championStats "anivia"
    putStrLn $ show $ s 
