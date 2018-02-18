module Day9 (d9) where
import Text.Parsec
import Advent2017

data StreamElement = 
    Group [StreamElement]
        | Garbage String deriving (Eq, Show)
garbage = do
    char '<' 
    gc <- concat <$> many garbageChar
    char '>'
    return $ Garbage gc



garbageChar = ((:[]) <$> (alphaNum <|> oneOf "{}<\"'," <|> space)) <|> cancelled

cancelled :: Parsec String () String
cancelled =  char '!' >> anyChar >> return ""

group :: Parsec String () StreamElement
group = do
    char '{'
    gp <- sepBy streamElement (spaces >> char ',' >> spaces)
    char '}'
    return $ Group gp
streamElement :: Parsec String () StreamElement
streamElement = group <|> garbage

gpCount :: StreamElement -> Integer
gpCount (Garbage _) = 0
gpCount (Group ss) = 1 + sum (map gpCount ss)

gpScore :: StreamElement -> Integer
gpScore = go 1
    where go n (Group ss) = n + sum (map (go (n + 1)) ss)
          go n (Garbage _) = 0

gbCount (Garbage g) = length g
gbCount (Group g) = sum (map gbCount g)

d9 = do 
    df <- dataFile "day9.txt"
    case parse streamElement "day9" df of
        Left err -> print $ "error: " ++ show err
        Right s -> do
            print $ "Count = " ++ show (gpCount s)
            print $ "Score = " ++ show (gpScore s)
            print $ "Garbage = " ++ show (gbCount s)


