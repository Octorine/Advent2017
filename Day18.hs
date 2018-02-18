module Day18 where
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
        | ADD Char Arg
        | MUL Char Arg
        | MOD Char Arg
        | RCV Char
        | JGZ Arg Arg deriving Eq
    data Arg = IntArg Integer | CharArg Char deriving Eq

    instance Show Arg where
        show (CharArg c) = show c
        show (IntArg i) = show i

    getArg :: Arg -> Machine -> Integer
    getArg (IntArg i) _ = i
    getArg (CharArg c) m = fromMaybe 0 $ m ^? (env . ix c)

    instance Show Op where
        show (SND a) = "snd " ++ show a
        show (SET r a) = "set " ++ show r ++ " " ++ show a
        show (ADD c a) = "add " ++ show c ++ " " ++ show a
        show (MUL c a) = "mul " ++ show c ++ " " ++ show a
        show (MOD c a) = "mod " ++ show c ++ " " ++ show a
        show (RCV a) = "rcv " ++ show a
        show (JGZ c a) = "jgz " ++ show c ++ " " ++ show a
    
    nextStep = over program right
    


    oper :: (Integer -> Integer -> Integer) -> Char -> Arg -> Machine -> Machine 
    oper f c a m = nextStep . over (env . ix c) (`f` getArg a m) $ m
    
    p1step :: Op -> Machine -> Machine
    p1step (SND a) m = (nextStep . over stack (getArg a m:)) m 
    p1step (SET r a) m = nextStep . over env (M.insert r (getArg a m)) $ m
    p1step (ADD r a) m = oper (+) r a m
    p1step (MUL r a) m = oper (*) r a m
    p1step (MOD r a) m = oper mod r a m
    p1step (RCV r) m = case M.lookup r (view env m) of
        Nothing -> nextStep m
        Just 0 -> nextStep m
        Just x -> nextStep (over stack tail m)
    p1step (JGZ r a) m = case compare (getArg r m) 0 of
        GT -> over program (move (getArg a m)) m
        _ -> nextStep m

    move x z = 
        case compare x 0 of
            EQ -> z
            LT -> if beginp z then end z else move (x + 1) (left z)
            GT -> move (x - 1) (right z)
        
    initMachine :: [Op] -> Machine
    initMachine ops = Machine [] M.empty (Data.List.Zipper.fromList ops)

        
    evalp1 :: Machine -> (M.Map Char Integer, [Integer])
    evalp1 m = case cursor (view program m) of
        RCV a -> if getArg (CharArg a) m > 0 then
                (view env m, view stack m)
             else evalp1 (p1step (cursor (view program m))m)
        _ -> evalp1 (p1step (cursor (view program m)) m) 

    pgm = initMachine [SET 'a' (IntArg 10), SND (CharArg 'a'), ADD 'a' (IntArg (-1)), 
                       JGZ (CharArg 'a') (IntArg (-2)), RCV 'b']  
    pgm2 = initMachine . map readInst $ ["set a 1", "add a 2", "mul a a", "mod a 5", 
                                         "snd a", "set a 0", "rcv a", "jgz a -1", 
                                         "set a 1", "jgz a -2"]
    readArg :: String -> Arg
    readArg a = if all isAlpha a then CharArg (head a) else IntArg (read a)    
    
    readInst line = 
        case words line of
            ["snd", x]    -> SND (readArg x)
            ["set", x, y] -> SET (head x) (readArg y)   
            ["add", x, y] -> ADD (head x) (readArg y)
            ["mul", x, y] -> MUL (head x) (readArg y)
            ["mod", x, y] -> MOD (head x) (readArg y)
            ["rcv", x]    -> RCV (head x)
            ["jgz", x, y] -> JGZ (readArg x) (readArg y)

    d18p1 = do
        code <- map readInst . lines <$> dataFile "day18.txt"
        print . evalp1 . initMachine $ code
    blocked :: Machine -> Bool
    blocked m = 
        endp (view program m) 
        || case cursor (view program m) of
                RCV _ -> null (view stack m)
                _ -> False
    
    p2step :: Op -> (Machine, Machine) -> (Machine, Machine)
    p2step (SND a) = \mm@(m1, m2) -> mm `seq` m1 `seq` m2 `seq`
                            let msg = getArg a m1
                            in over _1 nextStep (over (_2 . stack) (++[msg]) mm)  
    p2step (SET r a) = over _1 (\ m -> nextStep . over env (M.insert r (getArg a m))$ m)
    p2step (ADD r a) = over _1 $ oper (+) r a
    p2step (MUL r a) = over _1 $ oper (*) r a
    p2step (MOD r a) = over _1 $ oper mod r a
    p2step (RCV r) =  
        \mm -> 
            mm `seq`
            let msg = head (view (_1 . stack) mm) 
            in over _1 (nextStep . over stack tail . over env (M.insert r msg)) mm
    p2step (JGZ r a) = \(m1, m2) ->
        case compare (getArg r m1) 0 of
            GT -> (over program (move (getArg a m1)) m1, m2)
            _  -> (nextStep m1, m2)
    
    evalp2 :: (Machine, Machine, Integer) -> (M.Map Char Integer, [Integer], M.Map Char Integer, [Integer], Integer)
    evalp2 (m1, m2, acc) | blocked m1 && blocked m2 =
            (view env m1, view stack m1, view env m2, view stack m2, acc)
    evalp2 (m1, m2, acc) | blocked m1 =
        let (m2', m1') = p2step (cursor (view program m2)) (m2, m1)
        in acc `seq` m1' `seq` m2' `seq` evalp2 (m1', m2', acc) 
    evalp2 (m1, m2, acc) = 
        let (m1', m2') = p2step (cursor (view program m1)) (m1, m2)
            updateAcc (SND _) = acc + 1
            updateAcc _ = acc
        in evalp2 (m1', m2', updateAcc (cursor (view program m1)))
    
    eval2io m1 m2 acc | blocked m1 && blocked m2 = do
        putStrLn "Results:"
        print (acc, m1, m2)
    eval2io m1 m2 acc | blocked m1 = do 
        print (acc, view env m1, "BLOCKED", view env m2, cursor (view program m2))
        let (m2', m1') = p2step (cursor (view program m2)) (m2, m1)
        eval2io m1' m2' acc
    eval2io m1 m2 acc = do
        let acc' = case cursor (view program m1) of
                    SND _ -> acc + 1
                    _ -> acc
            (m1', m2') = p2step (cursor (view program m1)) (m1, m2)
        print (acc, view env m1, cursor (view program m1), view env m2, cursor (view program m2))
        eval2io m1' m2' acc'

    pgm3 = initMachine $ map readInst ["snd 1", "snd 2", "snd p", "rcv a", "rcv b", "rcv c", "rcv d"]
    d18p2 = do
        code <- initMachine . map readInst . lines <$> dataFile "day18.txt"
        evalPrint code

    evalPrint m =  eval2io (over env (M.insert 'p' 1) m) (over env (M.insert 'p' 0) m) 0
