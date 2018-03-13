module BaseAyto
    (Premut (..)
    ,MatchResult (..)
    ,TruthBooth (..)
    ,Constrictor (..)
    ,perms
    ,constrictPerm
    ) where



data Premut = Premut [String] deriving Show
--man, woman, perfect or not
data TruthBooth = TruthBooth Int String Bool deriving Show

data MatchResult = MatchResult Int Premut deriving Show

data Constrictor = TruthConstrict TruthBooth |
    MatchConstrict MatchResult deriving Show


--increaseIfNotMax :: [Int] -> Maybe [Int]
--increaseIfNotMax [] = Nothing
--increaseIfNotMax (x:xs)
--    | x == maxPerson = fmap ((:) 0) (increaseIfNotMax xs)
--    | otherwise = Just ((x + 1):xs)
--
--
--generatePremut :: Premut -> Maybe Premut
--generatePremut (Premut list) = fmap (Premut) (increaseIfNotMax list)
--
addOneIfEqual :: String -> String -> Int
addOneIfEqual a b
    | a == b = 1
    | otherwise = 0

calculateCorrect :: [String] -> [String] -> Int
calculateCorrect [] [] = 0
calculateCorrect (a:as) (b:bs) = (addOneIfEqual a b) + (calculateCorrect as bs)
calculateCorrect a b = error ("Got " ++ (show a) ++ " and " ++ (show b))

constrictPerm :: Constrictor -> [String] -> Bool
constrictPerm (TruthConstrict (TruthBooth man woman True)) list = (list !! man) == woman
constrictPerm (TruthConstrict (TruthBooth man woman False)) list = (list !! man) /= woman
constrictPerm (MatchConstrict (MatchResult correct (Premut list2))) list = (calculateCorrect list list2) == correct



perms :: [a] -> [ [a] ]
perms (a:as) = [bs ++ a:cs | perm <- perms as, (bs,cs) <- splits perm]
perms []     = [ [] ]
-- ways of splitting a list into two parts
splits :: [a] -> [ ([a],[a]) ]
splits []     = [ ([],[]) ]
splits (a:as) = ([],a:as) : [(a:bs,cs) | (bs,cs) <- splits as]


--data MatchResult = MatchResult Integer Integer Bool
someFunc :: IO ()
someFunc = putStrLn "someFunc"
