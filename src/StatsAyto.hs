module StatsAyto
    (Stats
    ,increaseName
    ,calcOneStat
    ,emptyStats
    ,combineStats
    ,calculateStats
    ,getName
    ) where

import Data.Map (Map)
import qualified Data.Map as Map


type Stats = Map (String, Int) Int

getName :: Stats -> String -> Int -> Int
getName stats str int = case Map.lookup (str, int) stats of
    Just x -> x
    Nothing -> 0

increaseName :: Stats -> Int -> String -> Stats
increaseName stats index name = case Map.lookup (name, index) stats of
    Just x -> Map.insert (name, index) (x + 1) stats
    Nothing -> Map.insert (name, index) 1 stats



calcOneStat :: Stats -> [String] -> Stats
calcOneStat stats list = fst $ foldl (\str (stats, x) -> (increaseName stats x str, x + 1)) (stats, 0) list
combineStats :: Stats -> Stats -> Stats
combineStats = Map.unionWith (+)
calculateStats :: [[String]] -> Stats
calculateStats [] = emptyStats
calculateStats (x:xs) = calcOneStat (calculateStats xs) x


emptyStats :: Stats
emptyStats = Map.empty
--calculateStats :: [[String]] -> Stats
--calculateStats (x:xs)
