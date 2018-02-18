module Day12 (d12) where

import           Advent2017
import Graph
import qualified Data.Map as M
import qualified Data.Set as S
import           Text.Parsec
d12Input :: Parsec String () (String, [String])
d12Input = do
    node <- many alphaNum
    spaces
    string "<->"
    spaces
    neighbors <- sepBy (many alphaNum) $ string ", "
    return (node, neighbors)
    
d12 = do
    txt <- lines <$> dataFile "day12.txt"
    case mapM (parse d12Input "d12") txt of
        Left err -> print err
        Right records -> let g = foldr (\ (n, ns) -> attach (Node n) (map Node ns)) Graph.empty records
                         in print (length $ extent (Node "0") g, ctCliques g)    