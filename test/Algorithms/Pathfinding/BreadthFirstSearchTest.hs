module Algorithms.Pathfinding.BreadthFirstSearchTest where
    import Test.HUnit
    import Graph
    import Algorithms.Pathfinding.BreadthFirstSearch
    
    -- Graph looks like:
    --         /--------\     
    --        /          v
    -- 0 --> 1 --> 3     4
    --  \                ^
    --   \----> 2 ------/

    node0 = (¤) 0
    node1 = (¤) 1
    node2 = (¤) 2
    node3 = (¤) 3
    node4 = (¤) 4

    edgeList = [Edge node0 node1, 
                Edge node0 node2, 
                Edge node1 node3, 
                Edge node1 node4, 
                Edge node2 node4]
    graph = constructGraph edgeList

    testBFS = 
        TestList [
            breadthFirstSearch graph node0 node4 ~?= [node0,node1,node4],
            breadthFirstSearch graph node0 node3 ~?= [node0,node1,node3],
            breadthFirstSearch graph node3 node4 ~?= [] -- There is no possible path here
        ]

    testsBFS =
        TestList [
            testBFS
        ]
        
    main = runTestTT testsBFS