module Graph
  ( empty
  , toList
  , fromEdges
  , toNode
  , dfs
  , bfs
  , dijkstra
  , dijkstra'
  , children
  , removeEdges
  )
where

import           Data.List (find)

data Graph = Graph [Node] [Edge] deriving (Show, Eq)

data Edge
  = Edge Node Node Int
  deriving (Show, Eq, Ord)

data Node = Node
  { nodeId :: String
  , distance :: Int
  , previousNode :: Maybe Node
  }
  deriving (Show, Eq, Ord)

empty :: Graph
empty = Graph [] []

fromEdges :: [(String, String, Int)] -> Graph
fromEdges edges =
  foldl (\graph edge -> 
    (addEdge . toEdge) edge graph
  )
  empty
  edges

toEdge :: (String, String, Int) -> Edge
toEdge (id1, id2, weight) = Edge (toNode id1) (toNode id2) weight

toNode :: String -> Node
toNode id1 = Node
  { nodeId = id1
  , distance = maxBound :: Int
  , previousNode = Nothing
  }

addEdge :: Edge -> Graph -> Graph
addEdge edge@(Edge n1 n2 _) g@(Graph nodes edges) = 
  case find ((==) edge) edges of 
    Nothing -> 
      foldl (\graph node ->
        addNode node graph
      )
      (Graph nodes (edges ++ [edge]))
      [n1, n2]
    Just _ ->
      g

addNode :: Node -> Graph -> Graph
addNode n g@(Graph nodes edges) =
  case find ((==) n) nodes of
    Nothing ->
      Graph (nodes ++ [n]) edges
    Just _ ->
      g

toList :: Graph -> ([String] , [(String, String, Int)])
toList (Graph nodes edges) = 
  ( fmap (\n -> nodeId n) nodes
  , fmap (\(Edge n1 n2 w) -> (nodeId n1, nodeId n2, w)) edges
  )

dijkstra :: Graph -> Node -> Maybe [String]
dijkstra (Graph [] _) _ = Nothing
dijkstra graph@(Graph (startNode:_) _) query = 
  case 
    getShortestPath query
      ( nodes 
      $ dijkstra' 
        (initStartNode startNode) 
        (replaceNode (initStartNode startNode) graph) 
      )
  of
    Just path -> Just $ nodeId <$> path
    Nothing -> Nothing
 where
  nodes (Graph nodes' _edges) = nodes'
  initStartNode :: Node -> Node
  initStartNode n = n { distance = 0 }

dijkstra'
  :: Node
  -> Graph
  -> Graph
dijkstra' startNode graph =
  foldl
    (\g node -> dijkstra' node g)
    (updatedGraphDistances startNode graph)
    (neighborNodes startNode graph)

 where
  updatedGraphDistances startNode' graph'=
    foldl
      (\g edge -> (removeEdge edge) (updateDistance edge startNode' g))
      graph'
      (neighborEdges startNode' graph')
  

getShortestPath :: Node -> [Node] -> Maybe [Node]
getShortestPath query nodes =
  case find (\node -> node == query) nodes of
    Just node ->
      case previousNode node of
        Just previous ->
          (++ [node]) <$> (getShortestPath previous nodes)
        Nothing ->
          Just [node]
    Nothing ->
      Nothing

updateDistance :: Edge -> Node -> Graph -> Graph
updateDistance edge sourceNode graph =
  case mDestNode edge graph of
    Just destNode ->
      if distance sourceNode + (weight edge) < distance destNode then
          replaceNode
            destNode {
              distance = distance sourceNode + (weight edge)
            , previousNode = Just sourceNode
            }
            graph
      else
        graph

    Nothing -> graph

 where
  mDestNode (Edge _source dest _weight) (Graph nodes _) = find (\n ->  nodeId n == nodeId dest) nodes
  weight (Edge _source _dest weight') = weight'

replaceNode :: Node -> Graph -> Graph
replaceNode node (Graph nodes edges) =
  Graph
    (foldl
      (\nodes' n ->
        if nodeId node == nodeId n then
          nodes' ++ [node]
        else
          nodes' ++ [n]
      )
      []
      nodes
    )
    edges


bfs :: Graph -> Node -> Maybe [String]
bfs (Graph [] _) _ = Nothing
bfs (Graph (startNode:_) edges) query = 
  case bfs' query startNode edges [startNode] [] of
    Just path -> Just $ nodeId <$> path
    Nothing -> Nothing

bfs' :: Node -> Node -> [Edge] -> [Node] -> [(Node, [Node])] -> Maybe [Node]
bfs' _ _ _ [] [] = Nothing
bfs' query start edges path paths =
  if start == query then
    Just path
  else
    searchSubGraph
      query
      (removeEdges' start edges)
      (enqueueNodes start edges path paths)
      
 where
  searchSubGraph :: Node -> [Edge] -> [(Node, [Node])] -> Maybe [Node]
  searchSubGraph _ _ [] = Nothing
  searchSubGraph query' edges' ((start', path') : paths') =
    bfs' query' start' edges' path' paths'

  enqueueNodes :: Node -> [Edge] -> [Node] -> [(Node, [Node])] -> [(Node, [Node])]
  enqueueNodes start' edges' path' paths' =
    paths' ++ map
      (\connectedNode ->
          (connectedNode, path' ++ [connectedNode])
      )
      (children' start' edges')
  
dfs :: Graph -> Node -> Maybe [String]
dfs (Graph [] _) _ = Nothing
dfs (Graph (startNode:_) edges) query = 
  case dfs' query startNode edges [startNode] [] of
    Just path -> Just $ nodeId <$> path
    Nothing -> Nothing

dfs' :: Node -> Node -> [Edge] -> [Node] -> [(Node, [Node])] -> Maybe [Node]
dfs' _ _ _ [] [] = Nothing
dfs' query start edges path paths =
  if start == query then
    Just path
  else
    searchSubGraph
      query
      (removeEdges' start edges)
      (enqueueNodes start edges path paths)
      
 where
  searchSubGraph :: Node -> [Edge] -> [(Node, [Node])] -> Maybe [Node]
  searchSubGraph _ _ [] = Nothing
  searchSubGraph query' edges' ((start', path') : paths') =
    dfs' query' start' edges' path' paths'

  enqueueNodes :: Node -> [Edge] -> [Node] -> [(Node, [Node])] -> [(Node, [Node])]
  enqueueNodes start' edges' path' paths' =
    map
      (\connectedNode ->
          (connectedNode, path' ++ [connectedNode])
      )
      (children' start' edges')
    ++ paths'

children :: Graph -> Node -> [String]
children (Graph _ edges) node = nodeId <$> children' node edges

children' :: Node -> [Edge] -> [Node]
children'  node edges =
  (map (\(Edge _ node' _) -> node')) $
    (filter (\(Edge node' _ _) -> node' == node) edges)

neighborNodes :: Node -> Graph -> [Node]
neighborNodes node (Graph nodes edges) =
  filter (\n -> elem (nodeId n) neighborNodes') nodes
  
 where
  neighborNodes' = 
    map
      (nodeId . destNode)
      (filter (\e -> nodeId (sourceNode e)  == nodeId node) edges)

  destNode (Edge _source dest _weight) = dest
  sourceNode (Edge source _dest _weight) = source

neighborEdges :: Node -> Graph -> [Edge]
neighborEdges node (Graph _nodes edges) =
  filter (\(Edge source _dest _weight) -> (nodeId node) == (nodeId source)) edges

removeEdges :: Graph -> Node -> Graph
removeEdges (Graph nodes edges) node = Graph nodes (removeEdges' node edges)

removeEdges' :: Node -> [Edge] -> [Edge]
removeEdges' node edges = filter (\(Edge node' _ _) -> node' /= node) edges

removeEdge :: Edge -> Graph -> Graph
removeEdge edge (Graph nodes edges) =
  Graph
    nodes
    (filter (/= edge) edges)

