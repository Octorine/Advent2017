module Ring where
    data Ring a = Ring [a] [a] deriving (Show, Eq)

    empty = Ring [] []

    fromList :: [a] -> Ring a
    fromList = Ring []

    toList :: Ring a -> [a]
    toList (Ring tl hd) = hd ++ reverse tl

    insert :: a -> Ring a -> Ring a
    insert a (Ring tl hd) = Ring tl (a : hd)

    right1, left1 :: Ring a -> Ring a
    right1 (Ring l []) = Ring [] (reverse l)
    right1 (Ring l [x]) = Ring [] (reverse (x : l))
    right1 (Ring l (x : r)) = Ring (x : l) r
    
    left1 (Ring [] r) = Ring (reverse r) []
    left1 (Ring [x] r) = Ring (reverse (x : r)) []
    left1 (Ring (x : l) r) = Ring l (x : r)

    right, left :: Int -> Ring a -> Ring a
    right n r = iterate right1 r !! n
    
    left n r = iterate left1 r !! n

    current (Ring l r) = head (r ++ reverse l)