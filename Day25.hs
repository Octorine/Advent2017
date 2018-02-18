module Day25 where
    import Advent2017
    import Data.Char
    import Data.List (foldl')
    import Data.List.Zipper
    import qualified Data.Map as M
    import Text.Parsec

    data Machine = Machine {
        tape :: !(Zipper Int),
        state :: !Char
    } deriving (Eq, Show)

    newMachine :: Machine
    newMachine = Machine (insert 0 empty) 'A'

    check :: Machine -> Integer
    check =  fromIntegral . length . filter (==1) . toList . tape
    -- Current value * current state -> new value * offset * new state
    type Rules =  M.Map (Int, Char) (Int, Int, Char)
    
    -- current value * current state -> new value * offset * new state
    type Rule = ((Int, Char),  (Int, Int, Char))
    -- In state A:
    -- If the current value is 0:
    --   - Write the value 1.
    --   - Move one slot to the right.
    --   - Continue with state B.
    -- If the current value is 1:
    --   - Write the value 0.
    --   - Move one slot to the left.
    --   - Continue with state B.
    
    asLine :: String -> Parsec String () a -> String -> Parsec String () a
    asLine before parser after= do
        string before 
        x <- parser
        string after
        string "\n"
        return x
    
    parseState = oneOf ['A' .. 'Z']

    parseRule :: Parsec String () [Rule]
    parseRule = do
        cs <- asLine "In state " parseState ":"
        vs <- many1 parseValues
        string "\n"
        return $ map (\(cv, (nv, offset, ns)) -> ((cv, cs), (nv, offset, ns))) vs
    
    parseRules :: Parsec String () [Rule]
    parseRules = do
        string "Begin in state A.\n"
        string "Perform a diagnostic checksum after 12656374 steps.\n"
        string "\n"
        rs <- many1 parseRule
        return . concat $ rs

    parseValue = digitToInt <$> oneOf "01"    
    
    parseOffset = 
        const (-1) <$> string "left" <|> const 1 <$> string "right"
    
    parseValues :: Parsec String () (Int, (Int, Int, Char))
    parseValues = do
        cv <- asLine  "  If the current value is " parseValue ":"
        nv <- asLine  "    - Write the value " parseValue "."
        off <- asLine "    - Move one slot to the " parseOffset  "."
        ns <- asLine  "    - Continue with state " parseState "."
        return (cv, (nv, off, ns))




    
    runRules :: Rules -> Machine -> Machine
    runRules rules m@(Machine tape state) =
        case M.lookup (cursor tape, state) rules of
            Nothing -> error "Invalid machine" -- ++ show m
            Just (newVal, offset , newState) -> Machine (move offset (replace newVal tape)) newState
    
    move :: Int -> Zipper Int -> Zipper Int
    move (-1) z = let z' = left z 
                  in if beginp z then insert 0 z' else z'
    move 0    z = z
    move 1    z = let z' = right z
                  in if endp z' then insert 0 z' else z'  
    move n    z = error $ "Illegal move " ++ show n

    eval :: Integer -> Rules -> (Integer, Machine)
    eval n rules = go n newMachine
        where go 0 m = (check m, m)
              go n m = m `seq` go (n - 1) $ runRules rules m

    d25p1 = do
        txt <- dataFile "day25.txt"
        case parse parseRules "d25" txt of 
            Left err -> print err
            Right rules -> print $ eval 12656374 (M.fromList rules)
