module Graph where

    -- Node --
    newtype Node = Node {
        nid :: Integer
    } deriving (Eq)

    instance Show Node where
        show node = show $ nid node
    ---------------------
    -- Edge --
    data Edge = Edge {
        src :: Node,
        end :: Node
    }

    -- Short-hand constructor for edge
    (<->) :: Node -> Node -> Edge
    (<->) node1 node2 = Edge node1 node2

    instance Show Edge where
        show edge = show (src edge) ++ " <-> " ++ show (end edge)

    instance Eq Edge where
        (==) (Edge a aa) (Edge b bb) = a == b && aa == bb
    -----------------------------------------------------------
    -- Graph --
    {-
        This graph is implemented in an adjacencylist structure
    -}

    --data Graph = 
