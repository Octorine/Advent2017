module Day16 where
import Advent2017
import Data.List
import Text.Parsec

data DanceStep = Spin Int | Exchange Int Int | Partner Char Char deriving (Eq, Show)

stepParser :: Parsec String () DanceStep
stepParser = spinParser <|> excParser <|> partnerParser

spinParser, excParser, partnerParser :: Parsec String () DanceStep
spinParser = do
    char 's'
    n <- read <$> many alphaNum
    return $ Spin n


excParser = do
    char 'x'
    a <- many alphaNum
    char '/'
    b <- many alphaNum
    return $ Exchange (read a) (read b)

partnerParser = do
    char 'p'
    a <- alphaNum
    char '/'
    b <- alphaNum
    return $ Partner a b

danceParser = sepBy stepParser (char ',')

dance :: String -> [DanceStep] -> String
dance = foldl' (flip step)

step :: DanceStep -> String -> String
step (Spin n) st = reverse . flop n . reverse $ st
step (Exchange a b) st | b < a = step (Exchange b a) st
step (Exchange a b) st | b == a = st
step (Exchange a b) st = take a st ++ [st!!b] ++ take (b - a - 1) (drop (a + 1) st) 
                                              ++ [st!!a] ++ drop (b + 1) st
step (Partner a b) st = map (partner a b) st

flop n s = drop n s ++ take n s

partner a b c | c == a = b
partner a b c | c == b = a
partner a b c = c

d16 =  do
    txt <- dataFile "day16.txt" 
    case parse danceParser "day16" txt of
        Left err -> print err
        Right steps -> print $ dance ['a' .. 'p'] steps
d16p2 = do
    txt <- dataFile "day16.txt" 
    case parse danceParser "day16" txt of
        Left err -> print err
        Right steps -> let m = findCycle ['a' .. 'p'] steps
                       in print $ dance ['a' .. 'p'] (concat $ replicate  (fromIntegral ((10^9) `mod` m)) steps)

findCycle :: String -> [DanceStep] -> Integer
findCycle s steps = go 1 s
    where go n s' = let s'' = dance s' steps
                 in if s'' == s then n else n `seq` go (n + 1) s''
