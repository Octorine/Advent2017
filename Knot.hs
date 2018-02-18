module Knot where
    import Data.Bits
    import Data.Char
    flipSome, skipSome :: Int -> [a] -> [a]


    flipSome n xs = reverse (take n xs) ++ drop n xs

    skipSome n xs =  let n' = n `mod` length xs
                     in drop n' xs ++ take n' xs

    chunkSome :: Int -> [a] -> [[a]]
    chunkSome _ [] = []
    chunkSome 0 xs = error "chunk size of 0" 
    chunkSome n xs = take n xs : chunkSome n (drop n xs)

    next (toFlip, toSkip) = skipSome (toFlip + toSkip) . flipSome toFlip


    hash lens n = let rotated = foldl (flip next) [0 .. n - 1] $ zip lens [0..]
                      rotation = sum (zipWith (+) lens [0..])
                      rotateBack = n - (rotation `mod` n)
                  in skipSome rotateBack rotated


    knot :: String -> [Int]
    knot s = let ending = [17, 31, 73, 47, 23]
                 nums = concat . replicate 64 . (++ending) . map ord $ s
                 sparse = hash nums 256
             in map (foldl xor 0) . chunkSome 16 $ sparse            
              