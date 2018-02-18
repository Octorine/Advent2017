module Day15 where
    import Format
    import Data.List
    import Data.Ord

    generator :: Integer -> Integer -> [Integer]
    generator factor seed = unfoldr (\s -> let n = (s * factor) `mod` 2147483647 in Just (n, n)) seed

    genA = generator 16807 65

    genB = generator 48271 8921

    lowBits :: Integer -> String
    lowBits = binShow 16 . (`mod` (2^16))

    compareGens :: [Integer] -> [Integer] -> Int -> Int
    compareGens a b n = length . filter (==EQ) $ zipWith (comparing lowBits) (take n a) (take n b) 
    compareGens' a b ad bd n = compareGens (filter (multipleOf ad) a) (filter (multipleOf bd) b) n
        where multipleOf d a = a `mod` d == 0
        

    d15 = compareGens (generator 16807 883) (generator 48271 879) (4 * 10^7)
    d15p2 = compareGens' (generator 16807 883) (generator 48271 879) 4 8 (5 * 10^6)