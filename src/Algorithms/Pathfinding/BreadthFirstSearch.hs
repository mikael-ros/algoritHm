module Algorithms.Pathfinding.BreadthFirstSearch where
    import Graph

    -- Starts the process
    breadthFirstSearch :: Eq t => Graph t -> Node t -> Node t -> [Node t]
    breadthFirstSearch graph start goal = bfs graph goal [(start, [start])] [start]

    -- This method sorts the predecessors of each node in PathMap (which is how it saves the path)
    bfs :: Eq t => Graph t -> Node t -> [PathMap t] -> [Node t] -> [Node t]
    bfs _ _ [] _ = nopath -- When queue is empty, we haven't found a path
    bfs graph goal queue@((q,path):qs) visited
        | q == goal = reverse path
        | otherwise = bfs graph goal newQueue (q:visited)
            where
                neighbors = getNodes (adjacency graph q)
                newQueue = qs ++ [(n, n:path) | n <- neighbors, n `notElem` visited]
 



