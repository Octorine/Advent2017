module Day14 where
    import Knot
    import Format
    import Control.Monad
    import Control.Monad.State
    import Data.Char
    import Data.List
    import qualified Data.Set as S
    import Numeric

    image = map (imageLine . knot) . makeKeys
        where makeKeys k = map (((k ++ "-") ++) . show) [0..127]
              imageLine = concatMap showBin
              showBin = binShow 8


    d14p1 = putStrLn . unlines . image $ "ljoxqyyw"

    d14p2 = print . subtract 1 . regions . minify . seed . image $ "ljoxqyyw"

    seed :: [String] -> [[Integer]]
    seed xss = evalState (mapM (mapM elt) xss) 1
        where elt :: Char -> State Integer Integer
              elt c = if c == '0' then return 0
                      else do
                        n <- get
                        put (n + 1)
                        return n

    minify'', minify', minify :: [[Integer]] -> [[Integer]]
    minify'' = map minLine'
        where minLine' = concatMap min' . groupBy zOrNot
              min' xs = replicate (length xs) (minimum xs) 
              zOrNot a b = ((a == 0) && b ==0) || (a /= 0 && b /= 0)
    minify' = minify'' . transpose . minify'' . transpose
    minify xss = let xss' = minify' xss
                 in if xss' == xss then xss else minify xss'

    regions :: Ord a => [[a]] -> Int
    regions = length . S.fromList . concat

    testGrid = ["1100100011",
                "1001100001",
                "1010000011",
                "1110111110"]

    exampleKey = "flqrgnkx"

