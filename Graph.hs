module Graph where
    import qualified Data.Set as S

    data Node = Node {nodeName :: String} deriving (Eq, Ord, Show);
    data Edge = Edge Node Node deriving (Eq, Ord, Show)

    data Graph = Graph {nodes :: S.Set Node, edges :: S.Set Edge} deriving (Eq, Show)


    addNode :: Node -> Graph -> Graph
    addNode n (Graph nodes edges) = Graph (S.insert n nodes) edges

    addEdge, addEdgeR :: Node -> Node -> Graph -> Graph

    addEdge n1 n2 (Graph nodes edges) = Graph nodes $ S.insert (Edge n1 n2) edges

    addEdgeR n1 n2 = addEdge n1 n2 . addEdge n2 n1

    nodeNames :: Graph -> [String]
    nodeNames = map nodeName . S.toList . nodes

    attach, attachR :: Node -> [Node] -> Graph -> Graph

    attach n neighbors g = let withNode = addNode n g
                            in foldr (addEdge n) withNode neighbors 

    attachR n neighbors  g= let withNode = addNode n g
                            in foldr (addEdgeR n) withNode neighbors

    remove n (Graph nodes neighbors) =
        Graph 
            (S.delete n nodes)
            (S.filter (\(Edge n1 n2) -> n1 /= n && n2 /= n) neighbors)

    neighbors :: Node -> Graph -> [Node]
    neighbors n (Graph _ ns) =
        S.toList . S.map (\(Edge _ n2) -> n2) 
                . S.filter (\(Edge n1 _) -> n1 == n) $ ns
    
    empty = Graph S.empty S.empty

    extent :: Node -> Graph -> S.Set Node
    extent n g = 
        let ns = neighbors n g
        in S.insert n . S.unions $ map (`extent` remove n g) ns


    ctCliques :: Graph -> Integer
    ctCliques g@(Graph n _) | S.null n = 0
    ctCliques g@(Graph n e) = 
        let n' = head (S.toList n)
            ext = extent n' g
            g' = foldr remove g ext
        in 1 + ctCliques g'