module Main where

import BaseAyto
import StatsAyto
import Data.Char
import Data.List as List
import Numeric
formatFloatN floatNum numOfDecimals = showFFloat (Just numOfDecimals) floatNum ""


numberOfCouples = 11

lineToResult :: [String] -> MatchResult
lineToResult list =
    (MatchResult (read (list !! numberOfCouples) :: Int) (Premut (take numberOfCouples list)))


inputToMatchResult :: String -> [MatchResult]
inputToMatchResult str = map (lineToResult . words) (lines str)

lineToTruth :: [String] -> TruthBooth
lineToTruth (num : (person : (result : []))) = (TruthBooth (read num) person (read result))

stringToConstrict :: String -> Constrictor
stringToConstrict str
    | str == "" = (error "got nothing")
    | (head (words str)) == "m" = (MatchConstrict (lineToResult (tail (words str))))
    | (head (words str)) == "t" = (TruthConstrict (lineToTruth (tail (words str))))



combinePredicates :: [ (a -> Bool) ] -> (a -> Bool)
combinePredicates (x:xs) = (\thing -> (x thing) && (combinePredicates xs thing))
combinePredicates [] = (\thing -> True)

--constrictListPerm :: [Constrictor] -> [[String]] -> [[String]]
--constrictListPerm x:xs aperms = constrictListPerm filter (constrictPerm x) aperms

possiblePerms :: [String] -> [String] -> [[String]]
possiblePerms defaultPerm constricts =
    let allPerms = perms defaultPerm
    in filter (combinePredicates (map (constrictPerm . stringToConstrict) constricts)) allPerms

--possbilePerms String -> [[String]]
--possbilePerms str =
--    let defs = lines str
--    in let defaultPerm = words $ head defs
--       in 

getDefault stuff = (words (head (lines (stuff))))
doParseAndJunk :: String -> [[String]]
doParseAndJunk stuff = possiblePerms (getDefault stuff)  (filter ((/=) "") (tail (lines (stuff))))

formatResults :: [[String]] -> [String] -> String
formatResults str def =
    "count: " ++ (show count) ++ "\n" ++
    (if count < 20
        then (List.intercalate "\n" (map (show) str)) ++ "\n"
        else "List too big to show\n")
    ++ (formatStats count (calculateStats str) def) ++ "\n"
    where count = length str



returnOdds :: Int -> Int -> Float
returnOdds x count = (fromIntegral x)/(fromIntegral count)

formatName :: Int -> Stats -> String -> String
formatName count stats name = name ++ " " ++ (foldr (\index str ->
    (formatFloatN (returnOdds (getName stats name index) count) 8)
    ++ " " ++ str) "" [0..10])


formatStats :: Int -> Stats -> [String] -> String
formatStats count stats list = List.intercalate "\n" (map (formatName count stats) list)

main :: IO ()
main = do
    inp <- getContents
    putStrLn (formatResults (doParseAndJunk inp) (getDefault inp))
