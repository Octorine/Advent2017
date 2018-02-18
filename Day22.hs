module Day22 where
    import Advent2017
    import qualified Data.Map as M
    import Control.Lens
    import Control.Monad.State.Strict

    data Direction = N | E | S | W deriving (Show, Eq)

    data NodeState = Cleaned | Weakened | Infected | Flagged deriving (Eq, Show)

    cw, ccw :: Direction -> Direction
    cw N = E
    cw E = S
    cw S = W
    cw W = N

    ccw N = W
    ccw E = N
    ccw S = E
    ccw W = S


    type Board = M.Map (Integer, Integer) NodeState
    
    data VirusState = 
        VS { board_ :: !Board,
             pos_ :: !(Integer, Integer),
             dir_ :: !Direction,
             score_ :: !Integer
           } deriving (Show, Eq)


    
    board :: Lens VirusState VirusState Board Board
    board f (VS b p d s) = (\b' -> VS b' p d s) <$> f b
    
    pos :: Lens VirusState VirusState (Integer, Integer) (Integer, Integer)
    pos f (VS b p d s ) =   (\p' -> VS b p' d s) <$> f p

    dir :: Lens VirusState VirusState Direction Direction
    dir f (VS b p d s) = (\d' -> VS b p d' s) <$> f d

    score :: Lens VirusState VirusState Integer Integer
    score f (VS b p d s) = VS b p d <$> f s

    readBoard :: [String] -> Board
    readBoard css = let w = length (head css)
                        wHalf = w `div` 2
                        h = length css
                        hHalf = h `div` 2
                    in foldr (uncurry M.insert) M.empty . filter ((/=Cleaned) . snd) $ do
                        x <- [0 .. w - 1]
                        y <- [0 .. h - 1]
                        return ((fromIntegral (x - wHalf), fromIntegral (hHalf - y)), 
                                 if (css !! y) !! x == '#' then Infected else Cleaned)

    doBurst = do
        infected <- (==Infected) <$> getCurrentStatus
        if infected then do
            unInfect
            modify (fwd . over dir cw)
        else do
            infect
            modify (fwd . over dir ccw . over score (+1))

    doBurst2 = do
        status <- getCurrentStatus
        currentScore <- view score <$> get
        case status of 
            Cleaned -> do
                setCurrent Weakened
                modify (fwd . over dir ccw)
            Infected -> do
                setCurrent Flagged
                modify (fwd . over dir cw)  
            Weakened -> do
                setCurrent Infected
                modify fwd
                currentScore `seq` modify (over score (+1))
            Flagged -> do
                removeCurrent
                modify (fwd . over dir (cw . cw))

            
        
    fwd :: VirusState -> VirusState
    fwd vs = case view dir vs of
        N -> over (pos . _2) (+1) vs
        S -> over (pos . _2) (subtract 1) vs
        W -> over (pos . _1) (subtract 1) vs
        E -> over (pos . _1) (+1) vs

    setCurrent :: NodeState -> State VirusState ()
    setCurrent ns = do
        p <- view pos <$> get
        modify (over board (M.insert p ns))

    --removeCurrent :: State VirusState ()
    removeCurrent = do
        p <- view pos <$> get
        modify (over board (M.delete p))

    --infect, unInfect :: State VirusState ()
    infect = setCurrent Infected

    unInfect = setCurrent Cleaned

    --getCurrentStatus :: State VirusState NodeState
    getCurrentStatus = do
        vs <- get
        case M.lookup (view pos vs) (view board vs) of
            Just s -> return s
            _ -> return Cleaned

    exBoard = readBoard $ lines "..#\n#..\n..."


    doTimes _ 0 = return ()
    doTimes thing n = thing >> doTimes thing (n - 1)
    
    d22p1 = do
        b <- readBoard . lines <$> dataFile "day22.txt"
        print . view score . snd $ runState (doTimes doBurst 10000) (VS b (0, 0) N 0)

    d22p2 = do
        b <- readBoard . lines <$> dataFile "day22.txt"
        print . view score . snd $ runState (doTimes doBurst2 10000000) (VS b (0, 0) N 0)


    