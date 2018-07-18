-- ##########
-- # GraHas #
-- ##########

-- author: Václav Balcar

-- Intro for users
-- GraHas is a shorter form of "graphs in Haskell" and as you can guess, it's a library for working with graphs in Haskell.

-- # Feature list:
-- * support of common graph representations
--   * adjacency matrix
--   * adjacency list
-- * generating graphs
--   * paths
--   * cycles
--   * complete bipartite graphs
--   * complete graphs
-- * getting graph's properties
--   * list of vertices
--   * list of edges
--   * list of connected components
--   * list of bridges in graph
--   * list of articulation points in graph
--   * list of cycles in graph
--   * hamiltonicity determination
-- * common graph algorithms
--   * searching in graph
--     * BFS
--     * DFS
--   * finding shortest path
--     * Dijkstra's algorithm
--     * Bellman-Ford algorithm
--     * Floyd-Warshall algorithm
--   * finding minimal spanning tree
--     * Borůvka's algorithm
--     * Jarník-Prim algorithm

-- More information about the library can be found in comments of its functions which represent documentation for programmers.
-- There's not much else to generally say about the GraHas but live long, prosper and spread the beauty of λ-calculus across
-- all known universe ;)

import Debug.Trace (trace)
import Data.List (sort, sortBy)

-- debug

-- | The 'deb' function writes out its argument and returns it.
deb :: Show a => a -> a
deb a = trace (show a) a

-- list processing

-- | The 'addToListComp' function adds an item to list, if it's not in it yet.
--   It takes custom equivalence function.
addToListComp :: (a -> a -> Bool) -> [a] -> a -> [a]
addToListComp _ [] b = [b]
addToListComp comp (a:as) b = if comp a b then a:as else a:(addToListComp comp as b)

-- | The 'addToList' function adds an item to list, if it's not in it yet.
--   It uses standard equivalence function.
addToList :: (Eq a) => [a] -> a -> [a]
addToList = addToListComp (==)

-- | The 'swapInListComp' function swaps an object with equivalent elements from a list in the list.
--   It takes custom equivalence function as an argument.
swapInListComp :: (a -> a -> Bool) -> [a] -> a -> [a]
swapInListComp _ [] _ = []
swapInListComp comp (a:as) b = if comp a b then b:as else a:(swapInListComp comp as b)

-- | The 'swapInList' function swaps an object with equivalent elements from a list in the list.
--   It uses standard equivalence function.
swapInList :: Eq a => [a] -> a -> [a]
swapInList = swapInListComp (==)

-- | The 'getFromListSel' function gets an object from list if the object is in it.
--   It takes custom selection function.
getFromListSel :: (a -> Bool) -> [a] -> Maybe a
getFromListSel _ [] = Nothing
getFromListSel sel (a:as) = if sel a then Just a else getFromListSel sel as

-- | The 'getFromList' function gets an object from list if the object is in it.
--   It uses standard equivalence function as the section function.
getFromList :: Eq a => a -> [a] -> Maybe a
getFromList t = getFromListSel (\a -> a == t)

-- | The 'listMemberComp' function checks if an object is member of a list.
--   It takes custom equivalence function.
listMemberComp :: (a -> a -> Bool) -> [a] -> a -> Bool
listMemberComp comp as a = (length as) == (length (addToListComp comp as a))

-- | The 'listMember' function checks if an object is member of a list.
--   It uses standard equivalence function.
listMember :: Eq a => [a] -> a -> Bool
listMember = listMemberComp (==)

-- | The 'listMinusComp' function is a (stable) set minus implementation for lists.
--   It takes custom equivalence function.
listMinusComp :: (a -> a -> Bool) -> [a] -> [a] -> [a]
listMinusComp comp as bs = filter (\a -> not (listMemberComp comp bs a)) as

-- | The 'listMinus' function is a (stable) set minus implementation for lists.
--   It uses standard equivalence function.
listMinus :: Eq a => [a] -> [a] -> [a]
listMinus = listMinusComp (==)

-- | The 'showList'' function converts a list to string.
--   It takes custom to-string-convertor for list's elements.
showList' :: (a -> String) -> [a] -> String
showList' sf l = foldr (\s1 s2 -> if s2 == [] then s1 else s1 ++ ", " ++ s2) [] (map sf l)

-- | The 'split' function splits a list to pair of list. 
--   The first list contains elements of the original list selected by selector, the second list contains others.
split :: (a -> Bool) -> [a] -> ([a], [a])
split sel as = ((filter sel as), (filter (not . sel) as))

-- | The 'extractFromList' function selects a object from list using selector and then deletes elements from list
--   which are in a relation with the selected element. 
--   This relation is described using deletor is a (stable) set minus implementation for lists.
--   It takes custom selector and deletor.
extractFromList :: (a -> Bool) -> (a -> a -> Bool) -> [a] -> Maybe (a, [a])
extractFromList sel del as =
    let mf = getFromListSel sel as
    in  if isNothing mf
        then Nothing
        else let f = fromJust mf
                 rs = filter (not . (del f)) as
             in  Just (f, rs)

-- | The 'structurize' function turns a list into list system, 
--   where list of Ints called structure describes the list system and satisfies that
--   ith number represent the length of ith list in the list system.
structurize :: [a] -> [Int] -> [[a]]
structurize as [] = []
structurize as (i:is) = (take i as):(structurize (drop i as) is)

-- | The minComp finds a minimum from a list using given custom simplified comparator.
minComp :: (a -> a -> Bool) -> [a] -> Maybe a
minComp comp (a:as) = Just (foldl (\cur next -> if (comp cur next) then next else cur) a as)
minComp _ _ = Nothing

-- | The 'pList' creates a power set of a list.
pList :: [a] -> [[a]]
pList [] = []
pList [a] = [[a], []]
pList (a:as) =
    let pAs = pList as
    in  pAs ++ (map (\s -> a:s) pAs)

-- | The 'update' function updates that elements from the first list, that equal to an element from the second list.
update :: Eq a => [a] -> [a] -> [a]
update o n = (listMinus o n) ++ n

-- maybe processing

-- | The 'isNothing' returns true for 'Nothing', false otherwise.
isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

-- | The 'fromJust' function extracts an object from Maybe type.
--   It throws an exception if the function is aplied on Nothing.
fromJust :: Maybe a -> a
fromJust (Just a) = a

--Int properties

-- | The 'sign' funtion return sign of given Int.
sign :: Int -> Int
sign 0 = 0
sign i = if i < 0 then -1 else 1

--tuple processing

-- | The 'trd' function return the third element from triple.
trd :: (a, b, c) -> c
trd (_, _, c) = c

-- | The 'tripleToPair' function converts a triple to pair by deleting the third element from the triple.
tripleToPair :: (a,b,c) -> (a,b)
tripleToPair (a,b,c) = (a,b)

--special constants

-- | 'infty' constant represents the positive countable infinity
infty :: Int
infty = maxBound :: Int

-- | 'negativeInfty' constant represents the negative countable infinity
negativeInfty :: Int
negativeInfty = minBound :: Int

-- | 'infinity' constant represents positive uncountable infinity
infinity :: Double
infinity = (read "Infinity") :: Double

-- Bounded implementation for Double bounding Doubles by positive and negative uncountable infinity
instance Bounded Double where
    minBound = -infinity
    maxBound = infinity

-- graph data types

-- type representing graph's vertices
data Vertex v e = Vertex {
    getID :: Int,
    getData :: v,
    getNeighboursList :: [(Int, e)]
}

-- Show implementation for Vertex
instance Show v => Show (Vertex v e) where
    show v = "v" ++ show (getID v) ++ " (" ++ (show . getData) v ++ ") " ++ (show (map fst (getNeighboursList v)))

-- Eq implementation for Vertex
instance Eq (Vertex v e) where
    v1 == v2 = (getID v1) == (getID v2)

-- Ord implementation for Vertex
instance Ord (Vertex v e) where
    compare v1 v2 = compare (getID v1) (getID v2)

-- just a shortcut for graph's edges
type Edge e = (Int, Int, e)

-- graph class definition
class Graph g where

    -- | The 'getVertices' function returns vertices of the graph.
    getVertices :: g v e -> [Vertex v e]
    
    -- | The 'getEdges' function returns edges of the graph.
    getEdges :: g v e -> [Edge e]

    -- | The 'getVertex' function returns vertex of the graph with the given id.
    getVertex :: g v e -> Int -> Maybe (Vertex v e)
    getVertex g = getVertex' (getVertices g)

    -- | The 'getEdge' function returns edge between vertices with given ids, if the edge exists.
    getEdge :: g v e -> Int -> Int -> Maybe (Edge e)
    getEdge g = getEdge' (getVertices g)

    -- | The 'getEdgeVal' function returns value on the edge between given vertices of the graph.
    getEdgeVal :: g v e -> Vertex v e -> Vertex v e -> Maybe e
    getEdgeVal g v1 v2 = getEdgeVal' (getVertices g) v1 v2
    
    -- | The 'getNeighbours' function returns neigbours a vertex with the given id in the graph.
    getNeighbours :: g v e -> Int -> Maybe [Vertex v e]
    getNeighbours g = getNeighbours' (getVertices g)

    -- | The 'fromVertices' function creates graph from list of vertices.
    fromVertices :: [Vertex v e] -> g v e

    -- | The 'safelyFromVertices' function safely (with fix of graph type if necessary) creates graph from list of vertices.
    safelyFromVertices :: [Vertex v e] -> g v e
    
    -- | The 'addVertex' function adds a vertex to the graph.
    addVertex :: g v e -> v -> g v e
    addVertex g v = fromVertices (addVertex' (getVertices g) v)
    
    -- | The 'updateVertex' function updates value of the vertex with the given id.
    updateVertex :: g v e -> Int -> v -> Maybe (g v e)
    updateVertex g i d = 
        let modifiedGraph = updateVertex' (getVertices g) i d 
        in  if isNothing modifiedGraph
            then Nothing
            else Just (fromVertices (fromJust modifiedGraph))

    -- | The 'setVVals' function sets values of vertices with members of given list. 
    --   Type of vertices data type can differ from the original one.
    setVVals :: g u e -> [v] -> g v e
    setVVals g nvs = fromVertices (setVVals' (getVertices g) nvs)
    
    -- | The 'addEdge' function adds an edge to the graph.
    addEdge :: g v e -> Edge e -> Maybe (g v e)

    -- | The 'addEdges' function adds edges from the given list to the graph.
    addEdges :: g v e -> [Edge e] -> Maybe (g v e)
    addEdges g [] = Just g
    addEdges g (e:es) =
        let modifiedGraph = addEdge g e
        in  if isNothing modifiedGraph
            then Nothing
            else addEdges (fromJust modifiedGraph) es
    
    -- | The 'deleteEdges' function deletes all edges from the graph.
    deleteEdges :: g v e -> g v e
    deleteEdges g = fromVertices (deleteEdges' (getVertices g))

    -- | The 'setEdgeVals' function sets values of edges with members of given list. 
    --   Type of vertices data type can differ from the original one.
    setEdgeVals :: g v f -> [e] -> g v e
    
    -- | The 'convert' function allows conversion from an instance of graph to another.
    convert :: Graph h => h v e -> g v e

    -- | The 'getStructure' function returns
    --   a list of Ints called structure describes the list system and satisfies that
    --   ith number represent the length of ith list in the list system.
    getStructure :: g v e -> [Int]
    getStructure g = getStructure' (getVertices g)

    -- | The 'sortByID' function sorts vertices and neighbours in neighbour list of each vertex of a graph by their IDs.
    sortByID :: g v e -> g v e
    sortByID g = fromVertices (sortByID' (getVertices g))

    -- | The 'normalize' returns a normalized form of a graph in which
    --   1) graph is sorted by IDs of vertices (see 'sortByID' function desciption)
    --   2) IDs of vertices are members of discrete interval [1..n], where n is number of vertices of the graph.
    normalize :: g v e -> g v e
    normalize g = fromVertices (normalize' (getVertices g))

    -- | The 'fromAdjacencyMatrix' function converts an adjacency matrix to a graph encoded into the matrix.
    fromAdjacencyMatrix :: [[Bool]] -> g (Maybe v) (Maybe e)

    -- | The 'toAdjacencyMatrix' function converts a graph to its adjacency matrix.
    toAdjacencyMatrix :: g v e -> [[Bool]]
    toAdjacencyMatrix g = toAdjacencyMatrix' (getVertices g)

-- | The 'getVertex'' function returns vertex of the graph with the given id.
getVertex' :: [Vertex v e] -> Int -> Maybe (Vertex v e)
getVertex' [] id = Nothing
getVertex' (v:vs) id = 
    if (getID v) == id
    then Just v
    else getVertex' vs id

-- | The 'getNeighbours'' function returns neigbours a vertex with the given id in the graph.
getNeighbours' :: [Vertex v e] -> Int -> Maybe [Vertex v e]
getNeighbours' vs i =
    let mv = getVertex' vs i
    in  if isNothing mv
        then Nothing
        else let v = fromJust mv
             in  Just (map fromJust (filter (not . isNothing) (map (\n -> getVertex' vs n) (map fst (getNeighboursList v)))))

-- | The 'getOutEdges'' function returns out edges of the vertex with given id.
getOutEdges' :: [Vertex v e] -> Int -> Maybe [Edge e]
getOutEdges' vs i = 
    let mv = getVertex' vs i
    in  if isNothing mv
        then Nothing
        else let v = fromJust mv
            in  Just (map (\(v2, d) -> (getID v, v2, d)) (getNeighboursList v))

-- | The 'getEdges'' function returns edges of the graph.
getEdges' :: [Vertex v e] -> [Edge e]
getEdges' vs = foldr (++) [] (map (\v -> fromJust (getOutEdges' vs v)) (map getID vs))

-- | The 'getEdge'' function returns edge between vertices with given ids, if the edge exists.
getEdge' :: [Vertex v e] -> Int -> Int -> Maybe (Edge e)
getEdge' vs i1 i2 =
    let mv1 = (getVertex' vs i1)
    in  if isNothing mv1
        then Nothing
        else let v1 = fromJust mv1
                 v1ID = getID v1
                 mv2 = getFromListSel (\(v2ID, _) -> v2ID == i2) (getNeighboursList v1)
             in  if isNothing mv2
                 then Nothing
                 else let v2 = fromJust mv2 
                      in  Just (v1ID, fst v2, snd v2)

-- | The 'getEdgeVal'' function returns value on the edge between given vertices of the graph.
getEdgeVal' :: [Vertex v e] -> Vertex v e -> Vertex v e -> Maybe e
getEdgeVal' vs v1 v2 = let edge = getEdge' vs (getID v1) (getID v2)
                      in  if isNothing edge
                          then Nothing
                          else Just (trd (fromJust edge))

-- | The 'addVertex'' function adds a vertex to the graph.
addVertex' :: [Vertex v e] -> v -> [Vertex v e]
addVertex' vs v = let id = (length vs) + 1
                  in  vs ++ [(Vertex id v [])]

-- | The 'updateVertex'' function updates value of the vertex with the given id.
updateVertex' :: [Vertex v e] -> Int -> v -> Maybe ([Vertex v e])
updateVertex' vs i d = 
    let mov = getVertex' vs i
    in  if isNothing mov
        then Nothing
        else let ov = fromJust mov
             in  Just (swapInList vs (Vertex i d (getNeighboursList ov)))

-- | The 'addEdge'' function adds an edge to the graph.
addEdge' :: [Vertex v e] -> Edge e -> Maybe ([Vertex v e])
addEdge' vs (v1, v2, e) = 
    let mfrom = getVertex' vs v1
    in  if isNothing mfrom
        then Nothing
        else let from = fromJust mfrom
             in  Just (swapInList vs (Vertex (getID from) (getData from) (addToListComp (\(w1, e1) (w2, e2) -> w1 == w2) (getNeighboursList from) (v2, e))))

-- | The 'addEdges'' function adds edges from the given list to the graph.
addEdges' :: [Vertex v e] -> [Edge e] -> Maybe ([Vertex v e])
addEdges' vs [] = Just vs
addEdges' vs (e:es) =
    let modifiedGraph = addEdge' vs e
    in  if isNothing modifiedGraph
        then Nothing
        else addEdges' (fromJust modifiedGraph) es

-- | The 'reverseEdge' function returns reversed edge of a given edge.
reverseEdge :: Edge e -> Edge e
reverseEdge (v1, v2, e) = (v2, v1, e)

-- | The 'addReverseEdges' adds reversed edge of each edge from a list of edges to the list, if it's not in the list.
addReverseEdges :: [Edge e] -> [Edge e]
addReverseEdges es = foldl (addToListComp (\(v1, v2, _) (u1, u2, _) -> v1 == u1 && v2 == u2)) es (map reverseEdge es)

-- | The 'deleteEdges'' function deletes all edges from the graph.
deleteEdges' :: [Vertex v e] -> [Vertex v e]
deleteEdges' vs = map (\(Vertex i d _) -> Vertex i d []) vs

-- | The 'getStructure'' function returns
--   a list of Ints called structure describes the list system and satisfies that
--   ith number represent the length of ith list in the list system.
getStructure' :: [Vertex v e] -> [Int]
getStructure' vs = map (length . getNeighboursList) vs

-- | The 'sortByID'' function sorts vertices and neighbours in neighbour list of each vertex of a graph by their IDs.
sortByID' :: [Vertex v e] -> [Vertex v e]
sortByID' vs = 
    let svs' = sort vs
        compareNs = \(id1, d1) (id2, d2) -> compare id1 id2
        sortNl = \(Vertex i d nl) -> (Vertex i d (sortBy compareNs nl))
    in  map sortNl svs'

-- | The 'normalize'' returns a normalized form of a graph in which
--   1) graph is sorted by IDs of vertices (see 'sortByID' function desciption)
--   2) IDs of vertices are members of discrete interval [1..n], where n is number of vertices of the graph.
normalize' :: [Vertex v e] -> [Vertex v e]
normalize' [] = []
normalize' vs =
    let (v:svs) = sortByID' vs
        v0ID = getID v
        tranform = \i' -> i' + (sign v0ID) * v0ID - 1
        tvs = map (\(Vertex i d nl) -> Vertex (tranform i) d (map (\(n,f) -> (tranform n, f)) nl)) (v:svs)
    in  map (\(v, newID) -> changeID v newID) (zip tvs [1..])

-- | The 'changeID' function changes id of a vertex.
changeID :: Vertex v e -> Int -> Vertex v e
changeID (Vertex i d nl) newID = 
    let setID = \id -> if id == i then newID else id
        unl = map (\(id, vd) -> (setID id, vd)) nl
    in  Vertex newID d unl

-- Data type 'DirectedGraph' represents directed graph.
data DirectedGraph v e = DirectedGraph {
    directedGraphVertices :: [Vertex v e]
}

-- implementation of Graph for DirectedGraph
instance Graph DirectedGraph where
    fromVertices vs = DirectedGraph vs
    safelyFromVertices = fromVertices
    getVertices g = directedGraphVertices g
    getEdges g = getEdges' (getVertices g)
    fromAdjacencyMatrix m = DirectedGraph (toGraph' m)
    addEdge g e = let modifiedGraph = addEdge' (getVertices g) e
                  in  if isNothing modifiedGraph
                      then Nothing
                      else Just (DirectedGraph (fromJust modifiedGraph))
    setEdgeVals g es = DirectedGraph (setEdgeVals' (getVertices g) es)
    convert g = DirectedGraph (getVertices g)

-- implementation of Show for DirectedGraph
instance (Show v, Show e) => Show (DirectedGraph v e) where
    show g = show (map show (getVertices g))

-- Data type 'UndirectedGraph' represents undirected graph.
data UndirectedGraph v e = UndirectedGraph {
    undirectedGraphVertices :: [Vertex v e]
}

-- implementation of Graph for UndirectedGraph
instance Graph UndirectedGraph where
    fromVertices vs = UndirectedGraph vs
    safelyFromVertices vs = UndirectedGraph (fromJust (addEdges' vs (map reverseEdge (getEdges' vs))))
    getVertices g = undirectedGraphVertices g
    getEdges g = 
        let edges = getEdges' (getVertices g)
        in  reverse (foldl (\cur next -> 
                if (listMemberComp (\(v1, v2, _) (v3, v4, _) -> v1 == v3 && v2 == v4) cur (reverseEdge next))
                then cur
                else next:cur) [] edges)
    fromAdjacencyMatrix m = safelyFromVertices (toGraph' m)
    addEdge g e = let modifiedGraph' = addEdge' (getVertices g) e
                      modifiedGraph = 
                          if isNothing modifiedGraph'
                          then Nothing 
                          else addEdge' (fromJust modifiedGraph') (reverseEdge e)
                  in  if isNothing modifiedGraph
                      then Nothing
                      else Just (UndirectedGraph (fromJust modifiedGraph))
    setEdgeVals g es = 
        let cg = UndirectedGraph (setEdgeVals' (getVertices g) es)
            newEdges = getEdges cg
            eg = deleteEdges cg
        in  fromJust (addEdges eg newEdges)
    convert g = safelyFromVertices (getVertices g)

-- implementation of Show for UndirectedGraph
instance (Show v, Show e) => Show (UndirectedGraph v e) where
    show g = show (map show (getVertices g))

-- | The 'showVertex' function shows id of a vertex.
showVertex :: Vertex v e -> String
showVertex v = show (getID v)

-- | The 'showVertices' function shows ids of vertices from a given list.
showVertices :: [Vertex v e] -> String
showVertices = showList' showVertex

-- | The 'showEdge' function converts an Edge to its String representation.
showEdge :: Show e => Edge e -> String
showEdge (v1, v2, e) = (show v1) ++ " -> " ++ (show v2) ++ " (" ++ (show e) ++ ") "

-- | The 'showEdges' function converts an list of Edges to its String representation.
showEdges :: Show e => [Edge e] -> String
showEdges = showList' showEdge

--undirected graph <-> adjacency matrix conversion

-- | The 'preprocessMatrix' function adds position indices to each member of a given matrix.
preprocessMatrix :: [[a]] -> Int -> [[(Int, Int, a)]]
preprocessMatrix m i = preprocessMatrix' m i i 

-- | The 'preprocessMatrix'' is an auxiliary function for the 'preprocessMatrix' function.
preprocessMatrix' :: [[a]] -> Int -> Int -> [[(Int, Int, a)]]
preprocessMatrix' [] _ _ = []
preprocessMatrix' (r:rs) i jDefault = (preprocessMatrixRow r i jDefault):(preprocessMatrix' rs (i + 1) jDefault)

-- | The 'preprocessMatrixRow' is an auxiliary function for the 'preprocessMatrix'' function.
preprocessMatrixRow :: [a] -> Int -> Int -> [(Int, Int, a)]
preprocessMatrixRow [] _ _ = []
preprocessMatrixRow (a:as) i j = (i, j, a):(preprocessMatrixRow as i (j + 1))

-- | The 'postprocessMatrix' function removes position indices from each member of a given matrix.
postprocessMatrix :: [[(Int, Int, a)]] -> [[a]]
postprocessMatrix m = map (\r -> map trd r) m

-- | The 'getFromMatrix' returns a member of matrix at given position if any.
getFromMatrix :: [[a]] -> Int -> Int -> Maybe a
getFromMatrix m i j = 
    let maxI = (length m) - 1
        maxJ = if maxI <= 0 then maxI else (length (m !! 0) - 1)
    in  if i > maxI || j > maxJ
        then Nothing
        else Just ((m !! i) !! j)

-- | The 'toGraph'' function converts an adjacency matrix to a graph encoded into the matrix.
toGraph' :: [[Bool]] -> [Vertex (Maybe v) (Maybe e)]
toGraph' am = map (\((from,to,val):ms) -> toVertex from (map tripleToPair (filter trd ((from,to,val):ms)))) (filter (/=[]) (preprocessMatrix am 1))

-- | The 'toVertex' function is an auxiliary function for the 'toGraph'' function.
toVertex :: Int -> [(Int, Int)] -> (Vertex (Maybe v) (Maybe e))
toVertex id nl = Vertex id Nothing (map (\(from, to) -> (to, Nothing)) nl)

-- | The 'toAdjacencyMatrix'' function converts a graph to its adjacency matrix.
toAdjacencyMatrix' :: [Vertex v e] -> [[Bool]]
toAdjacencyMatrix' vs = map (\r -> map (\m -> (m /= maxBound) && (m /= 0)) r) (toMatrix' (setEdgeVals' vs [1,1..]))

-- | The 'toMatrix'' function returns D0 (distance matrix of the 0th order) of the graph represented by a list of vertices.
toMatrix' :: (Bounded e, Num e) => [Vertex v e] -> [[e]]
toMatrix' vs =
    let l = length vs
    in  map (\(v, i) -> toMatrixRow i l v) (zip (normalize' vs) [1..])

-- | The 'toMatrixRow' function is an auxiliary function for the 'toMatrix'' function.
toMatrixRow :: (Bounded e, Num e) => Int -> Int -> Vertex v e -> [e]
toMatrixRow i l v =
    let incompleteRow = toMatrixRow' i 1 (getNeighboursList v)
    in  incompleteRow ++ (map (\k -> if i == k then 0 else maxBound) [((length incompleteRow) + 1)..l])

-- | The 'toMatrixRow'' function is an auxiliary function for the 'toMatrixRow' function.
toMatrixRow' :: (Bounded e, Num e) =>  Int -> Int -> [(Int, e)] -> [e]
toMatrixRow' _ _ [] = []
toMatrixRow' i k ((n,e):ns) = (map (\j -> 
    if i == j 
    then 0
    else (
        if j /= n
        then maxBound
        else e)) [k..n]) ++ (toMatrixRow' i (n + 1) ns)

--graph generating

-- | The 'newPn' function creates a directed Pn containing Nothings
newPn :: Int -> DirectedGraph (Maybe a) (Maybe b)
newPn n = DirectedGraph (map (\i -> (Vertex i Nothing (if i == n then [] else [(i + 1, Nothing)]))) [1..n])

-- | The 'newCn' function creates a directed Cn containing Nothings
newCn :: Int -> DirectedGraph (Maybe a) (Maybe b)
newCn n = DirectedGraph ((map (\i -> Vertex i Nothing [(i + 1, Nothing)]) [1..(n - 1)]) ++ [(Vertex n Nothing [(1, Nothing)])])

-- | The 'newKn' function creates a directed Kn containing Nothings
newKn :: Int -> DirectedGraph (Maybe a) (Maybe b)
newKn n = DirectedGraph (map (\i -> (Vertex i Nothing (map (\j -> (j, Nothing)) ([1..i - 1] ++ [i + 1..n])))) [1..n])

-- | The 'newKmn' function creates a directed Kmn containing Nothings
newKmn :: Int -> Int -> DirectedGraph (Maybe a) (Maybe b)
newKmn m n = DirectedGraph ((map (\i -> Vertex i Nothing (map (\j -> (j, Nothing)) [m + 1..m + n])) [1..m])
                    ++ (map (\i -> Vertex i Nothing (map (\j -> (j, Nothing)) [1..m])) [m + 1..m + n]))

-- graph editing

-- | The 'graph' function creates isomorphic graph to a given graph with values from 'vs' in vertices and values 'es' on edges.
graph :: (Graph h, Graph g) => h u f -> [v] -> [e] -> g v e
graph h vs es = 
    convert (DirectedGraph (do
        let oldVs = getVertices h
        ((Vertex id d nl), v, ces) <- zip3 oldVs vs (structurize es (getStructure h))
        return (Vertex id v (do
                ((nID, _), e) <- zip nl ces
                return (nID, e)
            ))
    ))

-- | The 'setVVals'' function sets values of edges with members of given list. 
--   Type of vertices data type can differ from the original one.
setVVals' :: [Vertex u e] -> [v] -> [Vertex v e]
setVVals' vs nvs = (map (\((Vertex i d nl), nd) -> Vertex i nd nl) (zip vs nvs))

-- | The 'setEdgeVals'' function sets values of edges with members of given list. 
--   Type of vertices data type can differ from the original one.
setEdgeVals' :: [Vertex v f] -> [e] -> [Vertex v e]
setEdgeVals' vs es = do        
        ((Vertex id d nl), ces) <- zip vs (structurize es (getStructure' vs))
        return (Vertex id d (do
                ((nID, _), e) <- zip nl ces
                return (nID, e)
            ))

-- common graph algorithms

-- The 'State' data type represents a state of a vertex in a phase of run of graph algorithm.
data State = Opened | Closed | Undiscovered deriving (Show, Eq)

-- just shortcuts for graph lookup function headers
type Lookup v e = [Vertex v e] -> [Vertex v e] -> Maybe [Vertex v e]
type EdgeLookup v e = [Edge e] -> [Vertex v e] -> Maybe [Vertex v e]
type GraphLookup g v w e = Int -> g v e -> Maybe [Vertex w e]

-- | The 'localGraphProcessing' function is a high order function which helps to create other 
--   graph processing functions which takes id of a starting vertex as an argument.
localGraphProcessing :: Graph g => GraphLookup g v w e -> ([Vertex w e] -> a) -> Int -> g v e -> (Maybe a)
localGraphProcessing lookup extract i g = 
    let lookedUp = lookup i g
    in  if isNothing lookedUp
        then Nothing
        else Just (extract (fromJust lookedUp))

-- | The 'graphLookup' function is a high order function which helps to create other graph lookup functions.
graphLookup :: Graph g => (v -> w) -> (w -> w) -> Lookup w e -> GraphLookup g v w e
graphLookup initVertexState initV0 lookup i g = 
    let initVertex = \(Vertex id d nl) -> Vertex id (initVertexState d) nl
    in  graphLookup' initV0 lookup i (map initVertex (getVertices g))

-- | The 'graphLookup'' function is an auxiliary function for the 'graphLookup' function.
graphLookup' ::(v -> v) -> Lookup v e -> Int -> [Vertex v e] -> Maybe [Vertex v e]
graphLookup' initV0 lookup i vs = 
    let mv0 = getVertex' vs i
    in  if isNothing mv0
        then Nothing
        else let (Vertex i d nl) = fromJust mv0
                 v0 = (Vertex i (initV0 d) nl)
             in  lookup vs [v0]

-- | The 'bfs' function applies bfs algorithm on a given graph.
bfs :: Graph g => GraphLookup g v (State, v) e
bfs = graphLookup (\v -> (Undiscovered, v)) (\(_, v) -> (Opened, v)) bfs'

-- | The 'bfs'' function is an auxiliary function for the 'bfs' function.
bfs' :: Lookup (State, v) e
bfs' r [] = Just r
bfs' vs (q:qs) = 
    let setState = \s (Vertex i (_, v) nl) -> Vertex i (s, v) nl
        mns = getNeighbours' (q:vs) (getID q)
    in  if isNothing mns
        then Nothing
        else let uns = map (setState Opened) (fromJust mns)
                 cq = setState Closed q
             in  let mcv = bfs' (listMinus vs (cq:uns)) (qs ++ uns)
                 in  if isNothing mcv
                     then Nothing
                     else Just (cq:(fromJust mcv))

-- | The 'discoverableVertices' function returns a list of discoverable vertices of a graph from the vertex with a given id.
discoverableVertices :: Graph g => Int -> g v e -> Maybe [Vertex v e]
discoverableVertices = 
    let getDiscovereable = \vs -> filter (\(Vertex id (state, _) nl) -> state == Closed) vs
        correctFormat = \(Vertex id (s, d) nl) -> Vertex id d nl
    in  localGraphProcessing bfs (\vs -> map correctFormat (getDiscovereable vs))

-- | The 'cc' function decomposes an undirected graph to its connected components.
cc :: UndirectedGraph v e -> [UndirectedGraph v e]
cc g = map (\vs -> UndirectedGraph vs) (cc' (getVertices g))

-- | The 'cc'' function decomposes an undirected graph to its connected components.
cc' :: [Vertex v e] -> [[Vertex v e]]
cc' [] = []
cc' (v:vs) = 
    let dvs = fromJust (discoverableVertices (getID v) (UndirectedGraph (v:vs)))
    in  dvs:(cc' (listMinus (v:vs) dvs))

-- | The 'isConnected' function determines whether a given undirected graph is connected.
isConnected :: UndirectedGraph v e -> Bool
isConnected g = (length (cc g)) == 1

-- | The 'dfs' function applies dfs algorithm on a given graph.
dfs :: Graph g => GraphLookup g v (State, v) e
dfs = graphLookup (\v -> (Undiscovered, v)) (\(_, v) -> (Opened, v)) dfs'

-- | The 'dfs'' function is an auxiliary function for the 'dfs' function.
dfs' :: Lookup (State, v) e
dfs' r [] = Just r
dfs' vs [q] = 
    let setState = \s (Vertex i (_, v) nl) -> Vertex i (s, v) nl
        mns = getNeighbours' (q:vs) (getID q)
    in  if isNothing mns
        then Nothing
        else let q' = setState Opened q
                 uns = filter (\(Vertex _ (s, _) _) -> s == Undiscovered) (fromJust mns)
             in  let sel = \closed next ->
                        if elem next closed
                        then closed
                        else closed ++ (fromJust (dfs' (q':(listMinus vs [q'])) [next]))
                     mcv = foldl sel [] uns
                     cq = setState Closed q'
                 in  Just (mcv ++ [cq])

-- | The 'bridgesDFS' function is a modified DFS used to find bridges in an undirected graph.
bridgesDFS :: GraphLookup UndirectedGraph v (State, Int, Int, Maybe Int, v) e
bridgesDFS = graphLookup (\v -> (Undiscovered, infty, infty, Nothing, v)) id (bridgesDFS' negativeInfty 0)

-- | The 'bridgeDFS'' function is an auxiliary function for the 'bridgeDFS' function.
bridgesDFS' :: Int -> Int -> Lookup (State, Int, Int, Maybe Int, v) e
bridgesDFS' p t vs [q] =
    let getState = \(Vertex _ (s, _, _, _, _) _) -> s
        setState = \s (Vertex id (_, i, low, br, d) nl) -> Vertex id (s, i, low, br, d) nl

        getVIn = \(Vertex id (_, i, _, _, _) nl) -> i
        setVIn = \newIn (Vertex id (s, i, low, br, d) nl) -> Vertex id (s, newIn, low, br, d) nl
        
        getVLow = \(Vertex id (_, _, low, _, _) _) -> low
        setVLow = \newLow (Vertex id (s, i, low, br, d) nl) -> Vertex id (s, i, newLow, br, d) nl
        
        setVBr = \newBr (Vertex id (s, i, low, br, d) nl) -> Vertex id (s, i, low, newBr, d) nl
        getVBr = \(Vertex id (_, _, _, br, _) _) -> br
        
        newT = t + 1
        q' = setState Opened (setVIn newT q)
        mns = getNeighbours' (q':vs) (getID q')
    in  if isNothing mns
        then Nothing
        else let qns = filter (\v -> (getState v) /= Closed) (fromJust mns)
                 (uvs, dvs) = split (\v -> (getVIn v) == infty) qns
                 sel = \closed next -> 
                     if elem next closed
                     then closed
                     else closed ++ (fromJust (bridgesDFS' (getID q') newT (q':(listMinus vs [q'])) [next]))
                 pvs = foldl sel [] uvs
                 upvs = map (\v -> 
                     if isNothing (getVBr v) 
                     then (setVBr (
                         if ((getVLow v) >= (getVIn v)) 
                         then Just (getID q')
                         else Nothing) v)
                     else v) pvs
                 newLow = min (getVLow q') (foldl min infty ((map getVLow upvs) ++ (map getVIn (filter (\v -> (getID v) /= p) dvs))))
                 cq = setState Closed (setVLow newLow q')
             in  Just (upvs ++ [cq])
bridgesDFS' _ _ _ _ = Nothing

-- | The 'bridges' function returns a list of bridges in an undirected graph.
bridges :: UndirectedGraph v e -> Maybe [Edge e]
bridges g =
    let vs = getVertices g
    in  if (length vs) == 0
        then Just []
        else let v = head vs
                 extract = foldl (\pvs (Vertex i (_, _, _, br, _) _) ->
                    if isNothing br
                    then pvs
                    else (fromJust (getEdge g i (fromJust br))):pvs) []
             in  localGraphProcessing bridgesDFS extract (getID v) g

-- | The 'apsDFS' function is a modified DFS used to find articulation points in an undirected graph.
apsDFS :: GraphLookup UndirectedGraph v (State, Int, Int, Bool, v) e
apsDFS = graphLookup (\v -> (Undiscovered, infty, infty, False, v)) id (apsDFS' negativeInfty 0)

-- | The 'apsDFS'' function is an auxiliary function for the 'apsDFS' function.
apsDFS' :: Int -> Int -> Lookup (State, Int, Int, Bool, v) e
apsDFS' p t vs [q] =
    let setState = \s (Vertex id (_, i, low, br, d) nl) -> Vertex id (s, i, low, br, d) nl
        getState = \(Vertex _ (s, _, _, _, _) _) -> s
        
        getVIn = \(Vertex id (_, i, _, _, _) _) -> i
        setVIn = \newIn (Vertex id (s, i, low, br, d) nl) -> Vertex id (s, newIn, low, br, d) nl
        
        getVLow = \(Vertex id (_, _, low, _, _) _) -> low
        setVLow = \newLow (Vertex id (s, i, low, br, d) nl) -> Vertex id (s, i, newLow, br, d) nl
        
        setVBr = \newBr (Vertex id (s, i, low, br, d) nl) -> Vertex id (s, i, low, newBr, d) nl
        getVBr = \(Vertex id (_, _, _, br, _) nl) -> br
        
        newT = t + 1
        q' = setState Opened (setVIn newT q)
        mns = getNeighbours' (q':vs) (getID q')
    in  if isNothing mns
        then Nothing
        else let qns = filter (\v -> (getState v) /= Closed) (fromJust mns)
                 (uvs, dvs) = split (\v -> (getVIn v) == infty) qns
                 sel = \closed next -> 
                     if elem next closed
                     then closed
                     else closed ++ (fromJust (apsDFS' (getID q') newT (q':(listMinus vs [q'])) [next]))
                 pvs = foldl sel [] uvs
                 ens = filter (\v -> (getVIn v) == (newT + 1)) pvs
                 q'' = if p == negativeInfty
                       then setVBr ((length ens) > 1) q'
                       else foldl (\cur next -> 
                           if (not (getVBr cur)) && (p /= (getID next))
                           then setVBr ((getVLow next) >= (getVIn q')) cur
                           else cur) q' ens
                 newLow = min (getVLow q'') (foldl min infty ((map getVLow pvs) ++ (map getVIn (filter (\v -> (getID v) /= p) dvs))))
                 cq = setState Closed (setVLow newLow q'')
             in  Just (pvs ++ [cq])
apsDFS' _ _ _ _ = Nothing

-- | The 'articulationPoints' function returns a list of articulation points in an undirected graph.
articulationPoints :: UndirectedGraph v e -> Maybe [Vertex v e]
articulationPoints g =
    let vs = getVertices g
    in  if (length vs) == 0
        then Just []
        else let  v = head vs
                  extract = foldl (\pvs (Vertex i (_, _, _, isAp, d) nl) -> 
                    if isAp
                    then (Vertex i d nl):pvs
                    else pvs) []
            in  localGraphProcessing apsDFS extract (getID v) g

-- | The 'cycles' function returns all cycles in a graph. 
--   The format of each cycle is same as a format of output of the 'toVertexSequence' function.
cycles :: UndirectedGraph v e -> [[Int]]
cycles g = map toVertexSequence (filter isCycle' (pList (listMinusComp (\e1 e2 -> (tripleToPair e1) == (tripleToPair e2)) (getEdges g) (addReverseEdges (fromJust (bridges g))))))

-- | The 'toVertexSequence' function transforms a set of edges defining a cycle to the around-the-cycle sorted set of ids
--   of vertices in the cycle.
--   If edges don't define just a cycle, output will be incorrect.
toVertexSequence :: [Edge e] -> [Int]
toVertexSequence [] = []
toVertexSequence [(_, v2, _)] = [v2]
toVertexSequence es =
    let ((v11, v12, e1):(v21, v22, e2):res) = sortCycle es
        commonV = if v11 == v21 || v11 == v22 then v11 else v12
        diffV = if commonV /= v11 then v11 else v12
    in  diffV:(toVertexSequence' commonV ((v21, v22, e2):res))

-- | The 'toVertexSequence'' function is an auxiliary function for the 'toVertexSequence' function.
toVertexSequence' :: Int -> [Edge e] -> [Int]
toVertexSequence' _ [] = []
toVertexSequence' cur ((v1, v2, _):es) =
    let next = if cur /= v1 then v1 else v2
    in  cur:(toVertexSequence' next es)

-- | The 'sortCycle' function sorts a list of edges to the around-the-cycle order.
--   If edges don't define just a cycle, output will be incorrect.
sortCycle :: [Edge e] -> [Edge e]
sortCycle ((v1,v2,e):es) = sortCycle' v2 ((v1,v2,e):es)

-- | The 'sortCycle'' function is an auxiliary function for the 'sortCycle' function.
sortCycle' :: Int -> [Edge e] -> [Edge e]
sortCycle' _ [] = []
sortCycle' v es = 
    let sel = \(u1, u2, _) -> (u1 == v) || (u2 == v)
        del = \e1 e2 -> (tripleToPair e1) == (tripleToPair e2)
        ((v1, v2, eVal), res) = fromJust (extractFromList sel del es)
        nextV = if v1 == v then v2 else v1
    in  (v1, v2, eVal):(sortCycle' nextV res)

-- | The 'isCycle' function determines whether a given undirected graph is a cycle.
isCycle :: UndirectedGraph v e -> Bool
isCycle g = isCycle' (getEdges g)

-- | The 'isCycle'' function determines whether a given undirected graph is a cycle.
isCycle' :: [Edge e] -> Bool
isCycle' [] = False
isCycle' ((v1, v2, e):es) = isCycle'' v1 v2 [v2] (addReverseEdges es)

-- | The 'isCycle''' function is an auxiliary function for the 'isCycle'' function.
isCycle'' :: Int -> Int -> [Int] -> [Edge e] -> Bool
isCycle'' cycleTarget currentTarget _ [] = cycleTarget == currentTarget
isCycle'' cycleTarget currentTarget usedV es =
    let sel = (\(v1', _, _) -> v1' == currentTarget)
        del = (\(v11', v12', _) (v21', v22', _) -> (v11' == v21' && v12' == v22') || (v11' == v22' && v12' == v21'))
        mf = extractFromList sel del es
    in  if isNothing mf
        then False
        else let ((_, v2, _), res) = fromJust mf
                in  (not (elem v2 usedV)) && isCycle'' cycleTarget v2 (v2:usedV) res

-- | The 'isHamiltonian' function determines whether a given undirected graph is hamiltonian.
isHamiltonian :: Eq e => UndirectedGraph v e -> Bool
isHamiltonian g =
    let v = length (getVertices g)
    in  foldl (\cur next -> cur || ((length next) == v)) False (cycles g)

-- just shortcuts for graph relaxation function headers
type RelaxVD v e = (State, e, Int, v)
type RelaxSelect v e = [Vertex (RelaxVD v e) e] -> Vertex (RelaxVD v e) e
type RelaxLookup v e = RelaxSelect v e -> Lookup (RelaxVD v e) e
type RelaxGraphLookup g v e = GraphLookup g v (RelaxVD v e) e

-- | The 'relax' function is a generic graph relaxation algorithm.
--   It needs a next-vertex-to-process-selection function as an argument to by apliable on a graph.
--   If you are looking for the simplest implementation of this algorithm, look at 'bellmanFord' function.
relax :: (Graph g, Bounded e, Real e) => RelaxSelect v e -> RelaxGraphLookup g v e
relax sel = graphLookup (\v -> (Undiscovered, maxBound, infty, v)) (\(_, _, _, v) -> (Opened, 0, infty, v)) (relax' sel)

-- | The 'relax'' function is an auxiliary function for the 'relax' function.
relax' :: Real e => RelaxLookup v e
relax' _ _ [] = Just []
relax' sel vs q =
    let getState = \(Vertex _ (s, _, _, _) _) -> s
        setState = \s (Vertex id (_, h, p, d) nl) -> Vertex id (s, h, p, d) nl

        getVH = \(Vertex _ (_, h, _, _) _) -> h
        setVH = \newH (Vertex id (s, h, p, d) nl) -> (Vertex id (s, newH, p, d) nl)
        
        setVP = \newP (Vertex id (s, h, p, d) nl) -> (Vertex id (s, h, newP, d) nl)
        
        minQ = sel q
        mns = getNeighbours' vs (getID minQ)
    in  if isNothing mns
        then Nothing
        else let ns = fromJust mns
                 uns = foldl (\acc v ->
                    let h = (fromJust (getEdgeVal' vs minQ v)) + (getVH minQ)
                    in  if ((getVH v) <= h)
                        then acc
                        else (setState Opened (setVP (getID minQ) (setVH h v))):acc
                    ) [] ns
                 cq = setState Closed minQ
             in  let uq = (listMinus q (cq:uns)) ++ uns
                     mcv = relax' sel (update vs (cq:uq)) uq
                 in  if isNothing mcv
                     then Nothing
                     else Just (cq:(fromJust mcv))

-- | The 'dijkstra' function applies Dijkstra's algorithm on a given graph.
dijkstra :: (Graph g, Bounded e, Real e) => RelaxGraphLookup g v e
dijkstra = 
    let getVH = \(Vertex _ (_, h, _, _) _) -> h
    in  relax (\q -> fromJust (minComp (\v1 v2 -> (getVH v1) > (getVH v2)) q))

-- | The 'bellmanFord' function applies Bellman-Ford algorithm on a given graph.
bellmanFord :: (Graph g, Bounded e, Real e) => RelaxGraphLookup g v e
bellmanFord = relax (\(q:qs) -> q)

-- | The 'shortestPathLength' function returns length of a shortest path in a given graph from a given vertex to the second given vertex.
--   Vertices are given via their ids.
shortestPathLength :: (Graph g, Bounded e, Real e) => Int -> Int -> g v e -> Maybe e
shortestPathLength = shortestPathLength' bellmanFord

-- | The 'shortestPathLength'' function is an auxiliary function for the 'shortestPathLength' function.
shortestPathLength' :: (Graph g, Bounded e, Real e) => (RelaxGraphLookup g v e) -> Int -> Int -> g v e -> Maybe e
shortestPathLength' lookup s t g = 
    let getVH = \(Vertex _ (_, h, _, _) _) -> h
        mLookupRes = lookup s g
        mTarget = getVertex g t
    in  if (isNothing mLookupRes) || (isNothing mTarget)
        then Nothing
        else Just (getVH (fromJust (getFromListSel (\v -> (getID v) == t) (fromJust mLookupRes))))

-- | The 'shortestPath' function returns a shortest path in a given graph from a given vertex to the second given vertex.
--   Vertices are given and returned via their ids.
shortestPath :: (Graph g, Bounded e, Real e) => Int -> Int -> g v e -> Maybe [Int]
shortestPath = shortestPath' bellmanFord

-- | The 'shortestPath'' function is an auxiliary function for the 'shortestPath' function.
shortestPath' :: (Graph g, Bounded e, Real e) => (RelaxGraphLookup g v e) -> Int -> Int -> g v e -> Maybe [Int]
shortestPath' lookup s t g = 
    let getVH = \(Vertex _ (_, h, _, _) _) -> h
        mLookupRes = lookup s g
        mTarget = getVertex g t
    in  if (isNothing mLookupRes) || (isNothing mTarget)
        then Nothing
        else Just (reverse (reconstructPath t (fromJust mLookupRes)))

-- | The 'reconstructPath' function reconstructs path from result of an relaxation algorithm.
--   Reconstructed path starts at vertex with given id.
reconstructPath :: (Bounded e, Real e) => Int -> [Vertex (RelaxVD v e) e] -> [Int]
reconstructPath i vs = 
    let getVP = \(Vertex _ (_, _, p, _) _) -> p
        v = fromJust (getVertex' vs i)
        p = getVP v
    in  if p == infty
        then [i] 
        else i:(reconstructPath p vs)

-- | The 'floydWarshall' function applies Floyd-Warshall algorithm on a given graph.
floydWarshall :: (Graph g, Bounded e, Real e) => g v e -> [[e]]
floydWarshall g = 
    let m = toMatrix' (getVertices g)    
    in  postprocessMatrix (floydWarshall' 0 ((length (getVertices g)) - 1) (preprocessMatrix m 0))

-- | The 'floydWarshall'' function is an auxiliary function for the 'floydWarshall' function.
floydWarshall' :: (Bounded e, Real e) => Int -> Int -> [[(Int, Int, e)]] -> [[(Int, Int, e)]]
floydWarshall' k mk am = 
    if k > mk
    then am
    else floydWarshall' (k + 1) mk (do
        r <- am
        return (do
                (i, j, m) <- r
                let mnvc1 = getFromMatrix am i (k + 1)
                    mnvc2 = getFromMatrix am (k + 1) j
                    nvc = if (isNothing mnvc1) || (isNothing mnvc2)
                          then maxBound
                          else let nvc1 = trd (fromJust mnvc1)
                                   nvc2 = trd (fromJust mnvc2)
                               in  if nvc1 == maxBound || nvc2 == maxBound
                                   then maxBound
                                   else nvc1 + nvc2
                return (i, j, min nvc m)
            )
        )

-- | The 'minSpanningTree' function returns minimum spanning tree of a given weighted graph.
minSpanningTree :: (Graph g, Bounded e, Real e) => g v e -> Maybe (g v e)
minSpanningTree g = 
    let getVP = \(Vertex _ (_, _, p, _) _) -> p
        jpRes = jarnikPrim g
        treeEdges = foldl (\cur v -> 
            let p = getVP v
                in if p == infty
                   then cur
                   else (fromJust (getEdge g (getID v) p)):cur) [] (fromJust jpRes)
    in  if isNothing jpRes
        then Nothing
        else addEdges (deleteEdges g) treeEdges

-- | The 'jarnikPrim' function applies Jarnik-Prim algorithm on a given graph.
jarnikPrim :: (Graph g, Bounded e, Real e) => g v e -> Maybe [Vertex (RelaxVD v e) e]
jarnikPrim g = 
    let vs = getVertices g
    in  if (length vs) == 0
        then Just []
        else graphLookup (\v -> (Undiscovered, maxBound, infty, v)) (\(_, _, _, v) -> (Opened, 0, infty, v)) jarnikPrim' (getID (head vs)) g

-- | The 'jarnikPrim'' function is an auxiliary function for the 'jarnikPrim' function.
jarnikPrim' :: Real e => Lookup (RelaxVD v e) e
jarnikPrim' _ [] = Just []
jarnikPrim' vs q =
    let getState = \(Vertex _ (s, _, _, _) _) -> s
        setState = \s (Vertex id (_, h, p, d) nl) -> Vertex id (s, h, p, d) nl

        getVH = \(Vertex _ (_, h, _, _) _) -> h
        setVH = \newH (Vertex id (s, h, p, d) nl) -> (Vertex id (s, newH, p, d) nl)
        
        setVP = \newP (Vertex id (s, h, p, d) nl) -> (Vertex id (s, h, newP, d) nl)
        
        minQ = fromJust (minComp (\v1 v2 -> (getVH v1) > (getVH v2)) q)
        mns = getNeighbours' vs (getID minQ)
    in  if isNothing mns
        then Nothing
        else let ns = fromJust mns
                 uns = foldl (\acc v ->
                     let h = getVH minQ
                     in  if ((getState v) == Closed) || ((getVH v) <= h)
                         then acc
                         else (setState Opened (setVP (getID minQ) (setVH h v))):acc
                     ) [] ns
                 cq = setState Closed minQ
                 in  let uq = (listMinus q (cq:uns)) ++ uns
                         mvs = jarnikPrim' (update vs (cq:uq)) uq
                     in  if isNothing mvs
                         then Nothing
                         else Just (cq:(fromJust mvs))

-- | The 'boruvka' function applies Borůvka's algorithm on a given graph.
boruvka :: Ord e => UndirectedGraph v e -> Maybe (UndirectedGraph v e)
boruvka g =
    let vs = getVertices g
        stvs = boruvka' (getEdges g) (deleteEdges' vs)
    in  if (not (isConnected g)) || isNothing stvs
        then Nothing
        else Just (UndirectedGraph (fromJust stvs))

-- | The 'boruvka'' function is an auxiliary function for the 'boruvka' function.
boruvka' :: Ord e => EdgeLookup v e
boruvka' _ [] = Just []
boruvka' es q =
    let forest = cc' q
    in  if (length forest) == 1
        then Just q
        else let ecs = map (\tree -> minComp (\(_, _, e1) (_, _, e2) -> e1 > e2) (filter (\(v1, v2, e) -> (elem v1 (map getID tree)) || (elem v2 (map getID tree))) es)) forest
                 aes = map fromJust (filter (not . isNothing) ecs)
                 res = boruvka' (listMinusComp (\(v11, v12, _) (v21, v22, _) -> (v11 == v21) && (v12 == v22)) es aes) (getVertices (fromJust (addEdges (UndirectedGraph q) aes)))
             in  if isNothing res
                 then Nothing
                 else res

-- | The 'testData' function returns test data.
testData :: [DirectedGraph (Maybe Int) (Maybe Int)]
testData = [
        newPn 5,
        addVertex (newPn 5) Nothing,
        newCn 5,
        newKn 5,
        newKmn 3 3,
        DirectedGraph [Vertex 1 Nothing [], Vertex 2 Nothing [], Vertex 3 Nothing []],
        DirectedGraph []
    ]

-- | The 'tests' function returns list of tests to be applied to 'testData'.
tests :: [(String, UndirectedGraph Char Double -> String)]
tests = [
        ("graph", show),
        ("vertices", show . getVertices),
        ("edges", show . getEdges),
        ("adjacency matrix", show . toAdjacencyMatrix),
        ("connected components", show . cc),
        ("bridges", show . bridges),
        ("articulation points", show . articulationPoints),
        ("cycles", show . cycles),
        ("isHamiltonian", show . isHamiltonian),
        ("bfs from 1", show . (bfs 1)),
        ("dfs from 1", show . (dfs 1)),
        ("Dijkstra's from 1", show . (dijkstra 1)),
        ("Bellman-Ford from 1", show . (bellmanFord 1)),
        ("Floyd-Warshall", show . floydWarshall),
        ("Jarnik-Prim", show . jarnikPrim),
        ("Boruvka", show . boruvka)
    ]

-- | The "runTests" function applies 'tests' on 'testData' and prints results to the stdin.
runTests :: IO ()
runTests =
    let toTestGraph = \g -> (graph g ['a'..] [1..])
    in  mapM_ putStrLn (do
            inputGraph <- testData
            let testGraph = toTestGraph inputGraph
            (name, test) <- tests
            return (name ++ ": " ++ (test testGraph))
        )