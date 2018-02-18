module Day21 where
    import Advent2017
    import Data.List
    import Data.List.Split
    import Data.Maybe

    data Rule = Rule [String] [String] deriving (Eq)

    square :: Int -> [[x]] -> [[[[x]]]]
    square n = map (map transpose . chunksOf n . transpose) . chunksOf n

    unsquare :: [[[[x]]]] -> [[x]]
    unsquare = mconcat .  map (transpose . mconcat . map transpose)

    match :: [String] -> Rule -> Maybe [String]
    match xss (Rule src dest) = 
        if src `elem` variations xss then
            Just dest
        else Nothing

    instance Show Rule where
        show (Rule one two) =
            unlines $ ("Rule: \t" ++ head one ++ arr ++ head two) :
                        zipWith showline (tail one) (tail two)
            where showline a b = "      \t" ++ a ++ arr ++ b
                  arr = "\t => \t"
   
    readRule :: String -> Rule
    readRule s = let [one, two] = map (splitOn "/") (splitOn " => " s)
                 in Rule one two


    variations xss = nub $ map ($xss)
        [id,
        transpose,
        reverse,
        transpose . reverse,
        reverse   . transpose,
        reverse   . transpose . reverse,
        transpose . reverse   . transpose,
        reverse   . transpose . reverse   . transpose,
        transpose . reverse   . transpose . reverse]


    initial :: [String]
    initial = [".#.",
                "..#",
                "###"]

    segment xss | length xss `mod` 2 == 0 = square 2 xss
    segment xss | length xss `mod` 3 == 0 = square 3 xss
    segment xss = error $ "Invalid sized patch " ++ show xss

    next :: [Rule] -> [String] -> [String]
    next rules patch = 
        let patchLn = length (show patch)
        in patchLn `seq` unsquare (map (map (\p -> head (mapMaybe (match p) rules))) (segment patch))


    d21 = do
        rules <- map readRule . lines <$> dataFile "day21.txt"
        print . length . filter (=='#') . mconcat $ iterate (next rules) initial !! 5      
        print . length . filter (=='#') . mconcat $ iterate (next rules) initial !! 18      