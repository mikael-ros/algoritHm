module GraphTest where
    import Test.HUnit
    import Graph
    
    testNode = nid (Node 2) ~?= 2

    node1 = Node 1
    node2 = Node 2

    edge1 = Edge node1 node2
    edge2 = node1 <-> node2
    edge3 = node2 <-> node1

    testEdge =
        TestList [
            edge1 == edge2 ~?= True,
            edge1 == edge3 ~?= False,
            show edge1 ~?= "1 <-> 2"
        ]

    testsGraph =
        TestList [
            testNode,
            testEdge
        ]
        
    main = runTestTT testsGraph