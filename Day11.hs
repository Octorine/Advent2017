module Day11 (d11) where
import Data.Ord
import Data.List
import Text.Parsec
import Control.Applicative (ZipList (..), (<*>))
import Advent2017

dirParser = do
    dirs <- sepBy (many alphaNum) (char ',') 
    many anyChar
    return dirs
data Pos = Pos !Int !Int !Int deriving (Eq, Show)


move :: String -> Pos -> Pos
move "n"  (Pos ne n nw) = Pos ne       (n + 1) nw
move "s"  (Pos ne n nw) = Pos ne       (n - 1) nw
move "ne" (Pos ne n nw) = Pos (ne + 1) n       nw
move "nw" (Pos ne n nw) = Pos ne       n       (nw + 1)
move "se" (Pos ne n nw) = Pos ne       n       (nw - 1)
move "sw" (Pos ne n nw) = Pos (ne - 1) n       nw

simpl (Pos ne n nw) | ne * nw > 0 && abs ne < abs nw =
    Pos 0 (n + ne) (nw - ne)
simpl (Pos ne n nw) | ne * nw > 0 = 
    Pos (ne - nw) (n + nw) 0
simpl (Pos ne n nw) | n * ne < 0 && abs n > abs ne =
    Pos 0 (n + ne) (nw - ne)
simpl (Pos ne n nw) | n * ne < 0 =
    Pos (ne + n) 0 (nw + n)
simpl (Pos ne n nw) | n * nw < 0 && abs n > abs nw =
    Pos (ne - nw) (n + nw) 0    
simpl (Pos ne n nw) | n * nw < 0 =
    Pos (ne + n) 0 (nw + n)     
simpl (Pos ne n nw) = Pos ne n nw

dist pos = let (Pos ne n nw) = simpl pos in abs ne + abs n + abs nw
d11 = do
    txt <- dataFile "day11.txt"
    case parse  dirParser "d11" txt of
        Left err -> print err
        Right dirs -> print $ foldr move (Pos 0 0 0) dirs

d11p2 = do
    txt <- dataFile "day11.txt"
    case parse  dirParser "d11" txt of
        Left err -> print err
        Right dirs -> let poss = scanl (flip move) (Pos 0 0 0) dirs
                          log = (,,) <$> ZipList dirs 
                                      <*> ZipList (simpl <$> poss) 
                                      <*> ZipList (dist <$> poss)
                      in print $ maximumBy (comparing dist) poss 