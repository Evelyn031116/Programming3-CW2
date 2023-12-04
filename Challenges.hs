{-# LANGUAGE DeriveGeneric #-}
-- comp2209 Functional Programming Challenges
-- (c) University of Southampton 2021
-- Skeleton code to be updated with your solutions
-- The dummy functions here simply return an arbitrary value that is usually wrong 

-- DO NOT MODIFY THE FOLLOWING LINES OF CODE
module Challenges (TileEdge(..),Tile(..),Puzzle,isPuzzleComplete,
                   Rotation(..),solveCircuit,
                   LExpr(..),Bind(..),prettyPrint,parseLetx,
                   LamExpr(..),letEnc,compareRedn)
                    where

-- Import standard library and parsing definitions from Hutton 2016, Chapter 13
import Parsing
import Data.List

-- Challenge 1
-- Testing Circuits

data TileEdge = North | East | South | West  deriving (Eq,Ord,Show,Read)
data Tile = Source [ TileEdge ] | Sink [ TileEdge ] | Wire [ TileEdge ]  deriving (Eq,Show,Read)
type Puzzle = [ [ Tile ] ]
type TileCoords = (Int, Int)

isPuzzleComplete :: Puzzle -> Bool
isPuzzleComplete puzzle = areWiresConnected puzzle && sourcesLinkedSinks puzzle && sinksLinkedSources puzzle

areWiresConnected :: Puzzle -> Bool
areWiresConnected puzzle = all (\coords -> all (isConnected coords) (getNeighborCoords coords)) wireCoords
  where
    wireCoords :: [TileCoords]
    wireCoords = [(i,j) | i <- [0..length puzzle - 1], j <- [0..length (head puzzle) - 1], isWire (puzzle !! i !! j)]
    getNeighborCoords :: TileCoords -> [TileCoords]
    getNeighborCoords (x, y) = filter isCoordValid [(x - 1, y), (x, y - 1), (x, y - 1), (x, y + 1)]
    isCoordValid :: TileCoords -> Bool
    isCoordValid (x, y) = x >= 0 && x < length puzzle && y < length (head puzzle)
    isConnected :: TileCoords -> TileCoords -> Bool
    isConnected coord1 coord2 = connectedEdge coord1 coord2 (getEdges (puzzle !! fst coord1 !! snd coord1)) (getEdges (puzzle !! fst coord2 !! snd coord2)) && notConnectedTo coord1 && notConnectedTo coord2 
    connectedEdge :: TileCoords -> TileCoords -> [TileEdge] -> [TileEdge] -> Bool
    connectedEdge (x1, y1) (x2, y2) edges1 edges2
      | x1 < x2 && South `elem` edges1 && North `elem` edges2 = True
      | x1 > x2 && North `elem` edges1 && South `elem` edges2 = True
      | y1 < y2 && East `elem` edges1 && West `elem` edges2 = True
      | y1 > y2 && West `elem` edges1 && East `elem` edges2 = True
      | x1 < x2 && South `notElem` edges1 && North `notElem` edges2 = True
      | x1 > x2 && North `notElem` edges1 && South `notElem` edges2 = True
      | y1 < y2 && East `notElem` edges1 && West `notElem` edges2 = True
      | y1 > y2 && West `notElem` edges1 && East `notElem` edges2 = True
      | otherwise = False

    notConnectedTo :: TileCoords -> Bool
    notConnectedTo (x, y)
      | x == 0 && North `elem` getEdges (puzzle !! x !! y) = False
      | x == length puzzle - 1 && South `elem` getEdges (puzzle !! x !! y) = False
      | y == 0 && West `elem` getEdges (puzzle !! x !! y) = False
      | y == length (head puzzle) && East `elem` getEdges (puzzle !! x !! y) = False

sourcesLinkedSinks :: Puzzle -> Bool
sourcesLinkedSinks puzzle1 
 | null sources = False
 | otherwise = all (\sink -> any (\source -> hasPath puzzle1 sink source)sources) sinks 
   where
     sources = [(i, j) | i <- [0..length puzzle1 - 1], j <- [0..length (head puzzle1) - 1], isSource (puzzle1 !! i !! j)]
     sinks= [(i, j) | i <- [0..length puzzle1 - 1], j <- [0..length (head puzzle1) - 1], isSink (puzzle1 !! i !! j)]

sinksLinkedSources :: Puzzle -> Bool
sinksLinkedSources puzzle1
  | null sinks = False
  | otherwise = all (\source -> any (\sink -> hasPath puzzle1 source sink)sinks) sources 
    where
      sources = [(i, j) | i <- [0..length puzzle1 - 1], j <- [0..length (head puzzle1) - 1], isSource (puzzle1 !! i !! j)]
      sinks= [(i, j) | i <- [0..length puzzle1 - 1], j <- [0..length (head puzzle1) - 1], isSink (puzzle1 !! i !! j)]

getEdges :: Tile -> [TileEdge]
getEdges (Source edges) = edges
getEdges (Sink edges) = edges
getEdges (Wire edges) = edges

isWire :: Tile -> Bool
isWire (Wire _) = True
isWire _ = False

isSource :: Tile -> Bool
isSource (Source _) = True
isSource _ = False

isSink :: Tile -> Bool
isSink (Sink _) = True
isSink _ = False

hasPath :: Puzzle -> TileCoords -> TileCoords -> Bool
hasPath puzzle1 coordsHead coordsTail = dfs coordsHead coordsTail []
  where
    dfs :: TileCoords -> TileCoords -> [TileCoords] -> Bool
    dfs currentCoord targetCoord visited
      | currentCoord == targetCoord = True
      | currentCoord `elem` visited = False
      | otherwise = any (\candidate -> dfs candidate targetCoord (currentCoord : visited)) candidates
        where
          candidates :: [TileCoords]
          candidates = filter (`notElem` visited) (getCandidates currentCoord)
          getCandidates :: TileCoords -> [TileCoords]
          getCandidates (x,y) =
            let edges = getEdges (puzzle1 !! x !! y)
            in [(x - 1, y) | North `elem` edges] ++
               [(x + 1, y) | South `elem` edges] ++
               [(x, y - 1) | West `elem` edges] ++
               [(x, y + 1) | East `elem` edges]

-- Challenge 2
-- Solving Circuits
data Rotation = R0 | R90 | R180 | R270 
  deriving (Eq,Show,Read)

solveCircuit :: Puzzle -> Maybe [[ Rotation ]]
solveCircuit = undefined

-- Challenge 3
-- Pretty Printing Let Expressions

data LExpr = Var Int | App LExpr LExpr | Let Bind  LExpr LExpr | Pair LExpr LExpr | Fst LExpr | Snd LExpr  | Abs Bind LExpr 
    deriving (Eq,Show,Read)
data Bind = Discard | V Int 
    deriving (Eq,Show,Read)

prettyPrint :: LExpr -> String
prettyPrint =  undefined


-- Challenge 4 - Parsing Let Expressions

parseLetx :: String -> Maybe LExpr
parseLetx = undefined

-- Challenge 5
-- Let Encoding in Lambda 

data LamExpr = LamVar Int | LamApp LamExpr LamExpr | LamAbs Int LamExpr 
                deriving (Eq, Show, Read)

letEnc :: LExpr -> LamExpr 
letEnc =  undefined

-- Challenge 6
-- Compare Innermost Reduction for Let_x and its Lambda Encoding

------------
-- LAMBDA --
------------

free :: Int -> LamExpr -> Bool
free x (LamVar y) =  x == y
free x (LamAbs y e) | x == y = False
free x (LamAbs y e) | x /= y = free x e
free x (LamApp e1 e2)  = (free x e1) || (free x e2)

rename :: Int -> LamExpr -> Int
rename x e | free (x+1) e = rename (x+1) e
           | otherwise = x+1 

subst :: LamExpr -> Int ->  LamExpr -> LamExpr
subst (LamVar x) y e | x == y = e
subst (LamVar x) y e | x /= y = LamVar x
subst (LamAbs x e1) y e  |  x /= y && not (free x e)  = LamAbs x (subst e1 y e)
subst (LamAbs x e1) y e  |  x /= y &&     (free x e)  = let x' = (rename x e1) in subst (LamAbs x' (subst e1 x (LamVar x'))) y e
subst (LamAbs x e1) y e  | x == y  = LamAbs x e1
subst (LamApp e1 e2) y e = LamApp (subst e1 y e) (subst e2 y e) 

isLamValue :: LamExpr -> Bool
isLamValue (LamVar _) = True
isLamValue (LamAbs _ _) = True
isLamValue _ = False

-- CALL BY VALUE -- 
cbvlam1 :: LamExpr -> Maybe LamExpr
-- Contexts
cbvlam1 (LamApp e1 e2) | not (isLamValue e1) = 
  do e' <- cbvlam1 e1
     return (LamApp e' e2)
cbvlam1 (LamApp e1 e2) | not (isLamValue e2) = 
  do e' <- cbvlam1 e2
     return (LamApp e1 e')
-- Reductions 
cbvlam1 (LamApp (LamAbs x e1) e) | isLamValue e = Just (subst e1 x e)
-- Otherwise terminated or blocked
cbvlam1 _ = Nothing

-- CALL BY NAME --
cbnlam1 :: LamExpr -> Maybe LamExpr
-- Reductions 
cbnlam1 (LamApp (LamAbs x e1) e) = Just (subst e1 x e)
-- Contexts
cbnlam1 (LamApp e1 e2) = 
  do e' <- cbnlam1 e1
     return (LamApp e' e2)
-- Otherwise terminated or blocked
cbnlam1 _ = Nothing

---------
-- LET --
--------- 



compareRedn :: LExpr -> Int -> (Int,Int,Int,Int)
compareRedn = undefined