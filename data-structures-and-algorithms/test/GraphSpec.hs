module GraphSpec where

import Test.Hspec

import qualified Graph as G


main :: IO ()
main = hspec spec

spec :: SpecWith ()
spec = do
  it "can create an empty graph - O(1)" $ do
    ((G.toList G.empty) :: ([String], [(String, String, Int)])) `shouldBe` ([],[])

  it "can find all child nodes" $ do
    G.children
      (G.fromEdges
        [ ("a", "b", 0)
        , ("a", "c", 0)
        , ("c", "d", 0)
        , ("b", "e", 0)
        , ("e", "d", 0)
        , ("d", "f", 0)
        , ("f", "a", 0)
        , ("f", "g", 0)
        ]
      )
      (G.toNode "f")
    `shouldBe` ["a", "g"]

  it "can remove all edges originating from a node" $ do
    G.toList 
      (G.removeEdges
        (G.fromEdges
          [ ("a", "b", 0)
          , ("a", "c", 0)
          , ("a", "d", 0)
          , ("a", "e", 0)
          , ("e", "d", 0)
          ]
        )
        (G.toNode "a")
      )
    `shouldBe`
    ( ["a", "b", "c", "d", "e"]
    , [ ("e", "d", 0)
      ]
    )

  it "can perform Depth First Search - O(V + E) time - O(V) space" $ do
    G.dfs
      (G.fromEdges
        [ ("a", "b", 0)
        , ("a", "c", 0)
        , ("c", "d", 0)
        , ("b", "e", 0)
        , ("e", "d", 0)
        , ("d", "f", 0)
        , ("f", "a", 0)
        , ("f", "g", 0)
        ]
      )
      (G.toNode "g")
    `shouldBe` (Just ["a", "b", "e", "d", "f", "g"])

  it "can perform Breadth First Search - O(V + E) time - O(VE) space (for tracking all paths)" $ do
    G.bfs
      (G.fromEdges
        [ ("a", "b", 0)
        , ("a", "c", 0)
        , ("c", "d", 0)
        , ("b", "e", 0)
        , ("e", "d", 0)
        , ("d", "f", 0)
        , ("f", "a", 0)
        , ("f", "g", 0)
        ]
      )
      (G.toNode "g")
    `shouldBe` (Just ["a", "c", "d", "f", "g"])

  --it "can perform Dijkstra's algorithm to find shortest path - O(V^2) time" $ do
    --G.toList
      --(G.dijkstra'
        --(G.toNode "start")
        --(G.fromEdges
          --[ ("start", "a", 0)
          --, ("a", "b", 5)
          --, ("a", "c", 2)
          --, ("c", "d", 10)
          --, ("b", "e", 1)
          --, ("e", "d", 8)
          --, ("d", "f", 2)
          --, ("f", "a", 3)
          --, ("f", "g", 2)
          --]
        --)
      --)
    --`shouldBe`
      --( []
      --, [ ("e", "d", 0)
        --]
      --)

  --it "can perform Dijkstra's algorithm to find shortest path - O(V^2) time" $ do
    --G.dijkstra
      --(G.fromEdges
        --[ ("a", "b", 5)
        --, ("a", "c", 2)
        --, ("c", "d", 10)
        --, ("b", "e", 1)
        --, ("e", "d", 8)
        --, ("d", "f", 2)
        --, ("f", "a", 3)
        --, ("f", "g", 2)
        --]
      --)
      --(G.toNode "g")
    --`shouldBe` (Just ["a", "c", "d", "f", "g"])
