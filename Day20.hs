module Day20 where
    import Advent2017
    import Data.List
    import Data.Ord
    import Text.Parsec


    type Vec = (Integer, Integer, Integer)
    
    data Particle = Particle {accel :: Vec, vel :: Vec, pos :: Vec} deriving (Eq, Show)

    parseParticle :: Parsec String () Particle
    parseParticle = do
        string "p=<"
        spaces
        pos <- parseVec
        string ">,"
        spaces
        string "v=<"
        spaces
        vel <- parseVec
        string ">,"
        spaces
        string "a=<"
        spaces
        acc <- parseVec
        string ">"
        spaces
        return $ Particle acc vel pos
    
    parseVec :: Parsec String () Vec
    parseVec = do
        x <- int
        string ","
        y <- int
        string ","
        z <- int
        return (x, y, z)

    int :: Parsec String () Integer
    int = do
        minus <- option "" (string "-")
        digs <- many1 digit
        return (read (minus ++ digs))

    evalPart (Particle (ax, ay, az) (vx, vy, vz) (px, py, pz)) t = 
            (eval ax vx px, eval ay vy py, eval az vz pz)
        where eval a v p = ((a * t * (t + 1)) `div` 2) + v * t + p

    manhatten (a, b, c) = abs a + abs b + abs c

    d20p1 = do
        txt <- lines <$> dataFile "day20.txt"
        let (Right parts) = mapM (parse parseParticle "day20") txt
        print . minimumBy (comparing (manhatten . flip evalPart 100000000 . snd)) . zip [0..] $ parts
    
    d20p2 = do
        txt <- lines <$> dataFile "day20.txt"
        let (Right parts) = mapM (parse parseParticle "day20") txt
        mapM_ (print . length) $ scanl f parts [1 .. 10000]
        where f ps n = map head . filter ((<=1) . length) . groupBy (posEq n) . sortBy (posComp n) $ ps
              posComp n = comparing ((\(x, y, z) -> [x, y, z]) . flip evalPart n) 
              posEq n p1 p2 = evalPart p1 n == evalPart p2 n
        

    