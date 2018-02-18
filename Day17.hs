module Day17 where
    import Advent2017
    import Ring
    spin :: Int -> a -> Ring a -> Ring a
    spin moves new = insert new . right1 . right moves
    

    d17p1 = current . right1 $ foldr (spin 304) (insert 0 empty) (reverse [1..2017])


    d17p2 top moves = go 1 1 1
        where go i pos acc = 
                if i == top then acc
                else let pos' = 1 + ((pos + moves) `mod` (i + 1))
                     in acc `seq` pos' `seq` go (i + 1) 
                                               pos' 
                                               (if pos' == 1 then i + 1 else acc)