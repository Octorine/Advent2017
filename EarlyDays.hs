module EarlyDays where
import Advent2017
import Data.Char
import Data.List
import Data.List.Zipper
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S
        
day1p1Input :: String
day1p1Input = "31813174349235972159811869755166343882958376474278437681632495222499211488649543755655138842553867246131245462881756862736922925752647341673342756514856663979496747158241792857625471323535183222497949751644488277317173496124473893452425118133645984488759128897146498831373795721661696492622276282881218371273973538163779782435211491196616375135472517935481964439956844536136823757764494967297251545389464472794474447941564778733926532741752757865243946976266426548341889873514383464142659425122786667399143335772174973128383869893325977319651839516694295534146668728822393452626321892357192574444856264721585365164945647254645264693957898373214897848424966266582991272496771159583715456714645585576641458358326521858518319315233857473695712238323787254556597566461188452279853766184333696344395818615215846348586541164194624371353556812548945447432787795489443312941687221314432694115847863129826532628228386894683392352799514942665396273726821936346663485499159141368443782475714679953213388375939519711591262489869326145476958378464652451441434846382474578535468433514121336844727988128998543975147649823215332929623574231738442281161294838499441799996857746549441142859199799125595761724782225452394593514388571187279266291364278184761833324476838939898258225748562345853633364314923186685534864178665214135631494876474186833392929124337161222959459117554238429216916532175247326391321525832362274683763488347654497889261543959591212539851835354335598844669618391876623638137926893582131945361264841733341247646125278489995838369127582438419889922365596554237153412394494932582424222479798382932335239274297663365164912953364777876187522324991837775492621675953397843833247525599771974555545348388871578347332456586949283657613841414576976542343934911424716613479249893113961925713317644349946444271959375981158445151659431844142242547191181944395897963146947935463718145169266129118413523541222444997678726644615185324461293228124456118853885552279849917342474792984425629248492847827653133583215539325866881662159421987315186914769478947389188382383546881622246793781846254253759714573354544997853153798862436887889318646643359555663135476261863"
day1p1len :: Int
day1p1len = length day1p1Input
rot :: Int -> [a] -> [a]
rot _ [] = []
rot 0 xs = xs
rot n (x:xs) = n `seq` xs `seq` rot (n - 1) $ xs ++ [x]

d1p1 :: Int
d1p1 = sum . map (digitToInt . fst) . filter (uncurry (==)) $ zip day1p1Input (rot 1 day1p1Input)


d1p2 :: String -> Int
d1p2 xs = sum . map (digitToInt . fst) . filter (uncurry (==)) $ zip xs (rot (length xs `div` 2) xs)



-------------------------------------------------------------------------------------------------------------
--Day 2

d2p1 :: ([Int] -> Int) -> IO ()
d2p1 ck = do
    inp <- (map (map read .words) . lines) <$> dataFile "d2p1.txt"
    print . sum . map ck $ inp

checksum1 :: [Int] -> Int
checksum1 xs = maximum xs - minimum xs

checksum2 :: [Int] -> Int
checksum2 xs = sum $ do
    x1 <- xs
    x2 <- xs
    return $ if x1 > x2 && x1 `mod` x2 == 0 then x1 `div` x2 else 0

-------------------------------------------------------------------------------------------------------------
--Day 3

squares :: [Integer]
squares = map (\n -> n * n) [1..]

rowDists :: Integer -> [Integer]
rowDists n = map eltDist [1..n - 1]
    where eltDist e = abs (e - nh) + nh
          nh = n `div` 2
spiralDists :: [Integer]
spiralDists = (:) 0 $ do
    shellNum <- [3, 5 ..]
    concat . replicate 4  $ rowDists shellNum

spirals :: [(Integer, Integer)]
spirals = iterate next (0, 0)
    where next (x, y) = case (x >= 0, y > 0, compare (abs x) (abs y)) of
                            (True, True, GT)   -> (x,     y + 1)
                            (True, True, _)    -> (x - 1, y)
                            (True, False, GT)  -> (x,     y + 1)
                            (True, False, _)   -> (x + 1, y)
                            (False, True, LT)  -> (x - 1, y)         
                            (False, True, _)   -> (x, y - 1)
                            (False, False, GT) -> (x, y - 1)
                            (False, False, _)  -> (x + 1, y)

spirals2 :: [(Integer, Integer)]
spirals2 = scanl addPair (0, 0) deltas
        where addPair (a1, b1) (a2, b2) = (a1 + a2, b1 + b2)
              deltas = do
                side <- [2, 4 ..]
                concat [[(1, 0)], 
                         replicate (side - 1) (0, 1), 
                         replicate side (-1, 0), 
                         replicate side (0, -1), 
                         replicate side (1, 0)]

neighbors :: (Integer, Integer) -> [(Integer, Integer)]
neighbors (x, y) = [(x - 1, y + 1), (x, y + 1), (x + 1, y + 1),
                    (x - 1, y),                 (x + 1, y),
                    (x - 1, y - 1), (x, y - 1), (x + 1, y - 1)]

spiralStressTest :: [M.Map (Integer, Integer) Integer]
spiralStressTest = scanl next (M.insert (0, 0) 1 M.empty) (tail spirals)
    where next m (x, y) = M.insert (x, y) (sum . catMaybes $ map (`M.lookup` m) (neighbors (x, y))) m
                                
d3p2 :: Integer
d3p2 = head . dropWhile (<289326) . catMaybes $ zipWith M.lookup spirals spiralStressTest

------------------------------------------------------------------------------------------------------------
--Day 4

d4p1 = do
    xss <- (map words . lines) <$> readFile "/home/james/Code/advent/2017/data/d4.txt"
    print . length . filter (\ln -> length ln == length (nub ln)) $ xss

d4p2 = do
    xss <- (map words . lines) <$> readFile "/home/james/Code/advent/2017/data/d4.txt"
    print . length . filter (\ln -> length ln == length (nub (map sort ln))) $ xss

 ------------------------------------------------------------------------------------------------------------
--Day 5

moveNext :: Zipper Int -> Maybe (Zipper Int, Zipper Int)
moveNext z =  go val (replace (val + 1) z)
    where   val = cursor z
            go spaces z = case compare spaces 0 of
                EQ -> Just (z, z)
                LT -> if beginp z then Nothing else go (spaces + 1) (left z)
                GT -> if endp (right z) then Nothing else go (spaces - 1) (right z)
            
day5p1 = do
    nums <- (map read . lines) <$> readFile "/home/james/Code/advent/2017/data/day5.txt"
    print $ 1 + length (unfoldr moveNext (fromList nums))

moveNext' :: Zipper Int -> Maybe (Zipper Int, Zipper Int)
moveNext' z = go val (replace (if val > 2 then val - 1 else val + 1) z)
    where   val = cursor z
            go spaces z = case compare spaces 0 of
                EQ -> Just (z, z)
                LT -> if beginp z then Nothing else go (spaces + 1) (left z)
                GT -> if endp (right z) then Nothing else go (spaces - 1) (right z)

day5p2 = do
    nums <- (map read . lines) <$> readFile "/home/james/Code/advent/2017/data/day5.txt"
    print $ 1 + length (unfoldr moveNext' (fromList nums))
                
------------------------------------------------------------------------------------------------------------
--Day 6
rightWrap :: Zipper a -> Zipper a
rightWrap z = if endp z || endp (right z) then start z else right z

dist :: Zipper Integer -> Zipper Integer
dist z = if endp z then z
         else go (cursor z) (rightWrap (replace 0 z))
    where go :: Integer -> Zipper Integer -> Zipper Integer
          go 0 z' = z'
          go n z' = go (n - 1) (rightWrap (replace (1 + cursor z') z'))

selectMax :: Zipper Integer -> Zipper Integer
selectMax z = let m = maximum (toList z)
              in go m (start z)
    where go m z' = if cursor z' == m then z' else go m (right z')

findCycle :: S.Set [Integer] -> Integer -> Zipper Integer -> Integer
findCycle s n z = if S.member (toList z) s then n
                  else findCycle (S.insert (toList z) s) (n + 1) (dist (selectMax z))

findCycle' :: M.Map [Integer] Integer -> Integer -> Zipper Integer -> (Integer, Integer)
findCycle' m n z = case M.lookup (toList z) m of
                        Just n' -> (n, n') 
                        Nothing -> findCycle' 
                                        (M.insert (toList z) n m) 
                                        (n + 1) 
                                        (dist (selectMax z))
                  

d6p1 = do
    z <- (fromList . map read . words) <$> readFile "/home/james/Code/advent/2017/data/day6.txt"
    print $ findCycle S.empty 0 z

d6p2 = do
    z <- (fromList . map read . words) <$> readFile "/home/james/Code/advent/2017/data/day6.txt"
    print $ findCycle' M.empty 0 z
    
 ------------------------------------------------------------------------------------------------------------
--Day 7

parentsAndChildren :: [[String]] -> S.Set String -> S.Set String -> (S.Set String, S.Set String)
parentsAndChildren [] p c = (p, c)
parentsAndChildren ((p : n : arr : tl) : rows) ps cs =
    parentsAndChildren rows (S.insert p ps) (foldr (S.insert . decomma) cs tl)
parentsAndChildren ([p, n] : tl) ps cs =
    parentsAndChildren tl (S.insert p ps) cs

decomma = filter (/=',')

familyTree [] m = m
familyTree ((p : n : arr : tl) : rows) tree = 
    familyTree rows (M.insert p (n, map decomma tl) tree)
familyTree ([p, n] : tl)  m = familyTree tl (M.insert p (n, []) m)


d7p1 = do
    input <- (map words . lines) <$> dataFile "day7.txt"
    let (p, c) = parentsAndChildren input S.empty S.empty
    print . filter (not . (`S.member` c)) $ S.toList p

d7p2 = do
    input <- (map words . lines) <$> dataFile "day7.txt"
    return $  familyTree input M.empty
treeSum :: String -> M.Map String (String, [String]) -> Integer
treeSum name m = case M.lookup name m of
        Nothing -> 0
        Just (n, children) -> read n + sum (map (`treeSum` m) children)