module Day23 where
    import Advent2017
    import qualified Data.Map as M
    import Data.Maybe
    import Control.Lens
    import Data.List.Zipper
    import Data.Char
    data Machine = Machine {stack_ :: ![Integer], env_ :: !(M.Map Char Integer), program_ :: !(Zipper Op)}
            deriving Show    
    stack :: Functor f => ([Integer] -> f [Integer]) -> Machine -> f Machine
    stack f (Machine s e p) = (\s' -> Machine s' e p) <$> f s

    env f (Machine s e p) = (\e' -> Machine s e' p) <$> f e

    program f (Machine s e p) = Machine s e <$> f p
    

    data Op = 
        SND Arg 
        | SET Char Arg
        | SUB Char Arg
        | MUL Char Arg
        | JNZ Arg Arg deriving Eq
    
    data Arg = IntArg Integer | CharArg Char deriving Eq

    instance Show Arg where
        show (CharArg c) = show c
        show (IntArg i) = show i

    getArg :: Arg -> Machine -> Integer
    getArg (IntArg i) _ = i
    getArg (CharArg c) m = fromMaybe 0 $ m ^? (env . ix c)

    instance Show Op where
        show (SET r a) = "set " ++ show r ++ " " ++ show a
        show (SUB c a) = "sub " ++ show c ++ " " ++ show a
        show (MUL c a) = "mul " ++ show c ++ " " ++ show a
        show (JNZ c a) = "jnz " ++ show c ++ " " ++ show a
    
    nextStep = over program right
    
    oper :: (Integer -> Integer -> Integer) -> Char -> Arg -> Machine -> Machine 
    oper f c a m = nextStep . over (env . ix c) (`f` getArg a m) $ m
  
    p1step :: Op -> Machine -> Machine
    p1step (SET r a) m = nextStep . over env (M.insert r (getArg a m)) $ m
    p1step (SUB r a) m = oper (flip subtract) r a m
    p1step (MUL r a) m = oper (*) r a m
    p1step (JNZ r a) m = case compare (getArg r m) 0 of
        EQ -> nextStep m
        _ -> over program (move (getArg a m)) m
        
    move x z = 
        case compare x 0 of
            EQ -> z
            LT -> if beginp z then end z else move (x + 1) (left z)
            GT -> move (x - 1) (right z)
        
    initMachine :: [Op] -> Machine
    initMachine ops = Machine [] M.empty (Data.List.Zipper.fromList ops)

        
    evalp1 :: Machine -> Integer -> Integer
    evalp1 m acc = 
        if endp (view program m) then acc
        else acc `seq` evalp1 
                (p1step (cursor (view program m))m)
                (case cursor (view program m) of
                    MUL _ _ -> acc + 1
                    _ -> acc)

    pgm = initMachine . map readInst $ ["set a 1", "sub a -2", "mul a a",  
                                         "set a 0", "jnz a -1", 
                                         "mul a 10"]
    readArg :: String -> Arg
    readArg a = if all isAlpha a then CharArg (head a) else IntArg (read a)    
    
    readInst line = 
        case words line of
            ["set", x, y] -> SET (head x) (readArg y)   
            ["sub", x, y] -> SUB (head x) (readArg y)
            ["mul", x, y] -> MUL (head x) (readArg y)
            ["jnz", x, y] -> JNZ (readArg x) (readArg y)

    d23p1 = do
        code <- map readInst . lines <$> dataFile "day23.txt"
        print . flip evalp1 0 . initMachine $ code
    
    -- d23p2 = do 
    --     code <- map readInst . lines <$> dataFile "day23.txt"
    --     return $ over env (M.insert 'a' 1) . initMachine $ code
    
    eio n m = 
        let op =  cursor (view program m)
            m' = p1step op m
            disp = ((if endp (view program m) then "End" else show op) ++ " \t\t" ++ concatMap distVal (M.toList (view env m)))
            distVal (k, v) = show k ++ ":" ++ show v ++ " \t"
        in if n == 0 || endp (right (view program m)) then putStrLn disp
           else do
            putStrLn disp
            eio (n - 1) m'
    
    isPrime n = all (\m -> n `mod` m /= 0) . takeWhile (\n' -> n' * n' <= n) $ [2..]
           
    d23p2 = let first = 107900
                last = 124900
            in length $ filter (not . isPrime) [first, first + 17 .. last]
    
    d23 = d23p1 >> print d23p2