module Day8 (d8p1, d8p2) where

import qualified Data.Map as M
import Control.Monad
import Control.Monad.State

dataFile :: String -> IO String
dataFile s = readFile $ "/home/james/Code/advent/2017/data/" ++ s

type RegisterState a = State (Integer, M.Map String Integer) a

readVar :: String -> RegisterState Integer
readVar v = do
    (mx, m) <- get
    case M.lookup v m of
        Nothing -> return 0
        Just n -> put (max mx n, m) >> return n

writeVar :: String -> Integer -> RegisterState ()
writeVar var val = do
    (mx, m) <- get
    put (max mx val, M.insert var val m)
modVar :: String -> (Integer -> Integer) -> RegisterState ()
modVar v f = readVar v >>= (writeVar v . f)

readInst :: [String] -> RegisterState ()
readInst [v1, act, n1, _, v2, rel, n2] =
    let op = case act of
            "inc" -> (+)
            "dec" -> (-)
        cmp = case rel of
            "<"  -> (<)
            ">"  -> (>)
            "<=" -> (<=)
            ">=" -> (>=)
            "==" -> (==)
            "!=" -> (/=) 
    in do
        val <- readVar v2
        when (val `cmp` read n2) $ modVar v1 (`op` read n1)
d8p1 = do 
    code <- map words . lines <$> dataFile "day8.txt"
    let ((), (mx, m)) = runState (mapM_ readInst code) (0, M.empty)
    print $ "HighWater = " ++ show mx
    print $ "Max = " ++ show (maximum . map snd $ M.toList m)



d8p2 = undefined
