module Day24 where
    import Advent2017
    import Data.List
    import Data.Ord

    type Port = (Integer, Integer)
    readPort :: String -> Port
    readPort s = (read (takeWhile (/='/') s), read (tail (dropWhile (/='/') s)))

    getPorts :: IO [Port]
    getPorts = map readPort . lines <$> dataFile "day24.txt"

    sumPorts :: [Port] -> Integer
    sumPorts p = sum (map fst p ++ map snd p)

    match b  (c, d) = b == c || b == d

    buildMatches a xs = if not (any (match a) xs) then [[]] else do
        (b, c) <- filter (match a) xs
        if b == a then
            ((b, c) :) <$> buildMatches c (xs \\ [(b, c)])
        else ((b, c) :) <$> buildMatches b (xs \\ [(b, c)])
    d24p1 = do
        ports <- map readPort . lines <$> dataFile "day24.txt"
        print . sumPorts . maximumBy (comparing sumPorts) $ buildMatches 0 ports
        
    d24p2 = do
        ports <- map readPort . lines <$> dataFile "day24.txt"
        print . sumPorts . maximumBy longStrong $ buildMatches 0 ports
        where longStrong a b = case comparing length a b of
                EQ -> comparing sumPorts a b
                cmp -> cmp