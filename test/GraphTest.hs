module GraphTest where
    import Test.HUnit
    import Graph
    
    testNode = content (Node 2) ~?= 2

    node1 = Node 1
    node2 = (¤) 2

    edge1 = Edge node1 node2
    edge2 = node1 <-> node2
    edge3 = node2 <-> node1

    testEdges = [edge1, edge2, edge3]

    testEdge =
        TestList [
            edge1 == edge2 ~?= True,
            edge1 == edge3 ~?= False,
            show edge1 ~?= "1 <-> 2"
        ]

    graph = Graph [(node1, [edge1, edge2]), (node2, [edge3])] [node1, node2] testEdges
    graph2 = constructGraph testEdges
    graph3 = constructUndirectedGraph testEdges

    testGraph =
        TestList [
            show graph ~?= "1 <-> 2 :: 1 <-> 2 :: 2 <-> 1",
            show graph == show graph2 ~?= True,
            nodes graph == nodes graph2 ~?= True,
            edges graph == edges graph2 ~?= True--,
            --edges graph3 /= edges graph2 ~?= True
        ]


    testsGraph =
        TestList [
            testNode,
            testEdge,
            testGraph
        ]
        
    main = runTestTT testsGraph