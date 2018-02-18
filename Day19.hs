module Day19 where
    import Advent2017
    import Control.Monad.State
    import Control.Monad.Writer
    import qualified Data.Array as A
    import Data.Char
    import Data.Monoid
    
    type Pos = (Int, Int)
    type Board = A.Array Pos Char

    data Direction = N | S | E | W deriving (Show, Eq)
    data Orientation = Horizontal | Vertical deriving (Show, Eq)

    orient :: Direction -> Orientation
    orient N = Vertical
    orient S = Vertical
    orient E = Horizontal
    orient W = Horizontal

    orientTrack '-' = Horizontal
    orientTrack '|' = Vertical

    sides d = filter (\d' -> orient d' /= orient d) [N, S, E, W]

    offset N = (0,  -1)
    offset S = (0, 1)
    offset E = (-1, 0)
    offset W = (1,  0)

    move :: Pos -> Direction -> Pos
    move p d = addVec p (offset d)

    readBoard :: String -> Board
    readBoard txt = let rows = lines txt
                        height = length rows
                        width = length (head rows)
                    in A.array ((1, 1), (height, width)) $ do
                        row <- zip [1..] rows
                        cell <- zip [1..] (snd row)
                        return ((fst cell, fst row), snd cell)
    findEntrance :: Board -> Pos 
    findEntrance b = head . filter (isTrack . (b A.!)) . map (flip (,) 1) $ [1 .. w]
        where ((1, 1), (h, w)) = A.bounds b
    
    isTrack :: Char -> Bool
    isTrack c = c == '|' || c == '-'
    
    addVec (a, b) (c, d) = (a + c, b + d)

    pathIter1 :: Board -> (Pos, Direction) -> Writer String (Pos, Direction)
    pathIter1 b (p, d) | isTrack (b A.! p) = 
        return (move p d, d) 
    pathIter1 b (p, d) | b A.! p == '+' = 
        return (head (map (\newDir -> (move p newDir, newDir)) (filter (\newDir -> isTrack (b A.! move p newDir)) (sides d))))
    pathIter1 b (p, d) | isAlpha (b A.! p) = tell [b A.! p] >> return (move p d, d) 
    pathIter1 b (p, d) =
         error $ "Invalid char" ++ show (b A.! p) ++ " at pos " ++ show p

    pathIter2 :: Board -> (Pos, Direction) -> Writer (Sum Integer) (Pos, Direction)
    pathIter2 b (p, d) | isTrack (b A.! p) || isAlpha (b A.! p) = 
        tell (Sum 1) >> return (move p d, d) 
    pathIter2 b (p, d) | b A.! p == '+' = do
        tell $ Sum 1
        return (head (map (\newDir -> (move p newDir, newDir)) (filter (\newDir -> isTrack (b A.! move p newDir)) (sides d))))
    pathIter2 b (p, d) =
         error $ "Invalid char" ++ show (b A.! p) ++ " at pos " ++ show p


    pathPrint :: (Show a, Monoid a) => Board -> (Pos, Direction) -> (Board -> (Pos, Direction) -> Writer a (Pos, Direction)) -> IO ()
    pathPrint b (p, d) f = print . runWriter $ go (p, d)
        where go (p, d) = do
                i <- f b (p, d)
                if b A.! fst i == ' ' then return i 
                else go i
    d19p1 = do
        b <- readBoard <$> dataFile "day19.txt"
        pathPrint b (findEntrance b, S) pathIter1
    d19p2 = do
        b <- readBoard <$> dataFile "day19.txt"
        pathPrint b (findEntrance b, S) pathIter2
    d19 = d19p1 >> d19p2