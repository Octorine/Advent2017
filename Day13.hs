module Day13 (d13) where 
    import Advent2017
    
    readLine :: String -> (Integer, Integer)
    readLine l = (read (takeWhile (/=':') l), read (tail (dropWhile (/= ':') l)))

    d13 = do
        nums <- (map readLine . lines) <$> dataFile "day13.txt"
        print (scoreTrip 0 nums)
        print (findFirst nums)
    
    scoreTrip :: Integer -> [(Integer, Integer)] -> Integer
    scoreTrip start  = sum . map (uncurry score . addStart)
        where addStart (a, b) = (a + start, b)
    
    findFirst :: [(Integer, Integer)] -> Integer
    findFirst nums = go 0
        where go n = if scoreTrip n nums == 0 then n
                     else go (n + 1)
    score :: Integer -> Integer -> Integer
    score depth range = 
        if (depth `mod` (2 * range - 2)) == 0 then 
            range * depth 
        else 0
     