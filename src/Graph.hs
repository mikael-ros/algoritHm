module Graph where

    -- Node --
    data Node t = Node {
        content :: t
    }

    (¤) :: t -> Node t
    (¤) content = Node content

    instance Show t => Show (Node t) where
        show node = show $ content node

    instance Eq t => Eq (Node t) where
        (==) (Node a) (Node b) = a == b
    ---------------------
    -- Edge --
    data Edge t = Edge {
        src :: Node t,
        end :: Node t
    }

    -- Short-hand constructor for edge
    (<->) :: Node t -> Node t -> Edge t
    (<->) node1 node2 = Edge node1 node2

    instance Show t => Show (Edge t) where
        show edge = show (src edge) ++ " <-> " ++ show (end edge)

    instance Eq t => Eq (Edge t) where
        (==) (Edge a aa) (Edge b bb) = a == b && aa == bb
    -----------------------------------------------------------
    -- Graph --
    {-
        This graph is implemented in an adjacencylist structure
    -}

    type AdjacencyList t = (Node t, [Edge t])

    data Graph t = Graph {
        adjacencies :: [AdjacencyList t],
        nodes :: [Node t],
        edges :: [Edge t]
    }

    constructUndirectedGraph :: Eq t => [Edge t] -> Graph t
    constructUndirectedGraph edges = Graph adjacencies nodes edges
        where 
            nodes = removeDuplicates $ concatMap (\e -> [src e, end e]) edges
            adjacencies = map (\node -> (node, [nedges | nedges <- edges, node `elem` [src nedges, end nedges]])) nodes

    constructGraph :: Eq t => [Edge t] -> Graph t
    constructGraph edges = Graph adjacencies nodes edges
        where 
            nodes = removeDuplicates $ concatMap (\e -> [src e, end e]) edges
            adjacencies = map (\node -> (node, [nedges | nedges <- edges, (src nedges) == node])) nodes

    removeDuplicates :: Eq a => [a] -> [a]
    removeDuplicates [] = []
    removeDuplicates (x:xs) = x : removeDuplicates (filter (/= x) xs)
                                
    instance Show t => Show (Graph t) where
        show graph = foldl (\acc edge -> if not (null acc) then acc ++ " :: " ++ edge else edge) [] $ map show (edges graph) -- a <-> b :: c <-> d :: ... :: y <-> z --

    --tupleLookup :: [(k,v)] -> k -> v
    tupleLookup tupleList key  = head $ filter (\tuple -> fst tuple == key) tupleList

    --adjacency :: Graph t -> Node t -> AdjacencyList t
    adjacency graph node = tupleLookup (adjacencies graph) node

    getNodes :: AdjacencyList t -> [Node t]
    getNodes adjacencies = map (end) (snd adjacencies)

    -------------------------------------------------------------
    -- Path --
    type Path t = [Node t]
    type PathMap t = (Node t, Path t)

    nopath :: Path t
    nopath = []