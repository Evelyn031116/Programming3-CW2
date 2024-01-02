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
import Data.Char
import Control.Applicative
import Control.Monad
import Data.Type.Equality (inner)
import Data.Functor.Classes (eq1)
import Foreign.C (e2BIG)

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

solveCircuit :: Puzzle -> Maybe [[Rotation]]
solveCircuit puzzle = undefined

{-dfs puzzle initialCondition

initialCondition :: Puzzle -> Condition
initialCondition puzzle = (puzzle, [], initialRotations)
  where
    initialRotations = replicate (length puzzle) (replicate (length (head puzzle)) R0)

dfs :: Puzzle -> Condition -> Maybe [[Rotation]]
dfs puzzle condition
    | isPuzzleComplete puzzle condition = Just (extractSolution condition)
    | isDeadEnd condition = Nothing
    | otherwise = explore puzzle condition

explore :: Puzzle -> Condition -> Maybe [[Rotation]]
type Condition = (Puzzle, [TileCoords], [[Rotation]])
explore puzzle condition = foldr (\rot acc -> acc <|> dfs (applyRotation puzzle rot) (updateCondition condition rot)) Nothing rotations
  where rotations = [R0, R90, R180, R270]

isDeadEnd :: Puzzle -> Bool
isDeadEnd puzzle = not $ areWiresConnected puzzle

applyRotation :: Puzzle -> TileCoords -> Rotation -> Puzzle
applyRotation puzzle (x, y) rotation =
    let (rowBefore, targetRow:rowAfter) = splitAt x puzzle
        (tilesBefore, targetTile:tilesAfter) = splitAt y targetRow
        rotatedTile = rotateTile targetTile rotation
    in rowBefore ++ [tilesBefore ++ [rotatedTile] ++ tilesAfter] ++ rowAfter

rotateTile :: Tile -> Rotation -> Tile
rotateTile tile R0 = tile
rotateTile (Source edges) rotation = Source (rotateEdges edges rotation)
rotateTile (Sink edges) rotation = Sink (rotateEdges edges rotation)
rotateTile (Wire edges) rotation = Wire (rotateEdges edges rotation)

rotateEdges :: [TileEdge] -> Rotation -> [TileEdge]
rotateEdges edges rotation = map (rotateEdge rotation) edges

rotateEdge :: Rotation -> TileEdge -> TileEdge
rotateEdge R90 North = East
rotateEdge R90 East = South
rotateEdge R90 South = West
rotateEdge R90 West = North
rotateEdge R180 North = South
rotateEdge R180 East = West
rotateEdge R180 South = North
rotateEdge R180 West = East
rotateEdge R270 North = West
rotateEdge R270 East = North
rotateEdge R270 South = East
rotateEdge R270 West = South
rotateEdge _ edge = edge-}






-- Challenge 3
-- Pretty Printing Let Expressions

data LExpr = Var Int | App LExpr LExpr | Let Bind  LExpr LExpr | Pair LExpr LExpr | Fst LExpr | Snd LExpr  | Abs Bind LExpr 
    deriving (Eq,Show,Read)
data Bind = Discard | V Int 
    deriving (Eq,Show,Read)

prettyPrint :: LExpr -> String

prettyPrint (Var i) = "x" ++ show i
prettyPrint (App e1 e2) = handleLeftApp e1 ++ " " ++ handleRightApp e2
prettyPrint (Let b e1 e2) = "let " ++ prettyBind b ++ " " ++ handleAbs e1 ++ " in " ++ prettyPrint e2
prettyPrint (Fst e) = "fst (" ++ prettyPrint e ++ ")"
prettyPrint (Snd e) = "snd (" ++ prettyPrint e ++ ")"
prettyPrint (Abs b e) = "\\" ++ prettyBind b ++ prettyPrintInner e
prettyPrint (Pair e1 e2) = "(" ++ prettyPrint e1 ++ ", " ++ prettyPrint e2 ++ ")"

prettyBind :: Bind -> String
prettyBind Discard = "_"
prettyBind (V i) = "x" ++ show i

handleAbs :: LExpr -> String
handleAbs (Abs b e) = prettyBind b ++ " " ++ handleAbs e
handleAbs (Let b e1 e2) = "= (" ++ prettyPrint (Let b e1 e2) ++ ")"
handleAbs e = "= " ++ prettyPrint e

prettyPrintInner :: LExpr -> String
prettyPrintInner (Abs b inner) = case b of
  Discard -> " _" ++ prettyPrintInner inner
  V i     -> " " ++ prettyBind b ++ prettyPrintInner inner
prettyPrintInner e = " -> " ++ prettyPrint e

handleLeftApp :: LExpr -> String
handleLeftApp (App e1 e2) = "(" ++ prettyPrint e1 ++ " in " ++ handleLeftApp e2
handleLeftApp (Var n) = prettyPrint (Var n)
handleLeftApp (Let b e1 e2) =
  case b of
    Discard -> "let _ = " ++ prettyPrint e1 ++ " in " ++ handleLeftApp e2
    V n     -> "let " ++ prettyBind b ++ " = " ++ prettyPrint e1 ++ " in " ++ handleLeftApp e2
handleLeftApp (Fst e) = prettyPrint (Fst e)
handleLeftApp (Snd e) = prettyPrint (Snd e)
handleLeftApp e = "(" ++ prettyPrint e ++ ")"

handleRightApp :: LExpr -> String
handleRightApp (App e1 e2) = "(" ++ prettyPrint e1 ++ " " ++ prettyPrint e2 ++ ")"
handleRightApp e = prettyPrint e

-- Challenge 4 - Parsing Let Expressions

parseLetx :: String -> Maybe LExpr
parseLetx input = case parse parseLExpr input of
    [(result, "")] -> Just result
    _           -> Nothing

parseLExpr :: Parser LExpr
parseLExpr = parseLet <|> parseAbs <|> parseApp <|> parsePair <|> parseVar <|> parseFst <|> parseSnd

parseBind :: Parser Bind
parseBind = do
  parseV <|> parseDiscard
  where
    parseV = do
      symbol "x"
      n <- natural
      return (V n)

    parseDiscard = do
      symbol "_"
      return (Discard)


parseVar :: Parser LExpr
parseVar = do
  symbol "x"
  n <- natural
  return (Var n)

parseAbs :: Parser LExpr
parseAbs = do
    symbol "\\"
    b <- some parseBind
    symbol "->"
    e <- parseLExpr
    return (foldr Abs e b)

removeBrakets :: Parser LExpr
removeBrakets = do
  symbol "("
  e <- parseLExpr
  symbol ")"
  return e

parseApp :: Parser LExpr
parseApp = do
  es <- some (parseVar <|> removeBrakets)
  return (foldl1 App es)

parseBrackets :: Parser LExpr
parseBrackets = do
  symbol "("
  e <- parseAbs
  symbol ")"
  return (e)

parseLet :: Parser LExpr
parseLet = do
    symbol "let"
    b <- parseBind
    bs <- many parseBind
    symbol "="
    e1 <- parseLExpr
    symbol "in"
    e2 <- parseLExpr
    return (Let b (foldrBE bs e1) e2)

foldrBE :: [Bind] -> LExpr -> LExpr
foldrBE [] e1 = e1
foldrBE b e1 = foldr Abs e1 b

parsePair :: Parser LExpr
parsePair = do
    symbol "("
    e1 <- parseLExpr
    symbol ","
    e2 <- parseLExpr
    symbol ")"
    return $ Pair e1 e2

parseFst :: Parser LExpr
parseFst = do
  symbol "fst"
  symbol "("
  e1 <- parseLExpr
  symbol ","
  e2 <- parseLExpr
  symbol ")"
  return (Fst e1)

parseSnd :: Parser LExpr
parseSnd = do
  symbol "snd"
  symbol "("
  e1 <- parseLExpr
  symbol ","
  e2 <- parseLExpr
  symbol ")"
  return (Snd e1)


-- Challenge 5
-- Let Encoding in Lambda 

data LamExpr = LamVar Int | LamApp LamExpr LamExpr | LamAbs Int LamExpr 
                deriving (Eq, Show, Read)

letEnc :: LExpr -> LamExpr
letEnc (Var n) = LamVar n
letEnc (App e1 e2) = LamApp (letEnc e1) (letEnc e2)
letEnc (Let Discard e1 e2) = let fresh = findNextUnused (extractFreeVars e2) in LamApp (LamAbs fresh (letEnc e2)) (letEnc e1)
letEnc (Let (V n) e1 e2) = LamApp (LamAbs n (letEnc e2)) (letEnc e1)
letEnc (Pair e1 e2) = let fresh = findNextUnused (extractFreeVars e1 ++ extractFreeVars e2) in LamAbs fresh (LamApp (LamApp (LamVar fresh)(letEnc e1)) (letEnc e2))
letEnc (Abs Discard e) = let fresh = findNextUnused (extractFreeVars e) in LamAbs fresh (letEnc e)
letEnc (Abs (V n) e) = LamAbs n (letEnc e)
letEnc (Fst e) = LamApp (letEnc e) (LamAbs 0 (LamAbs 1 (LamVar 0)))
letEnc (Snd e) = LamApp (letEnc e) (LamAbs 0 (LamAbs 1 (LamVar 1)))

findNextUnused :: [Int] -> Int
findNextUnused usedVars = head $ filter (`notElem` usedVars) [2..]

extractFreeVars :: LExpr -> [Int]
extractFreeVars (Var n) = [n]
extractFreeVars (App e1 e2) = extractFreeVars e1 ++ extractFreeVars e2
extractFreeVars (Let (V n) e1 e2) = n : (extractFreeVars e1 ++ extractFreeVars e2)
extractFreeVars (Pair e1 e2) = extractFreeVars e1 ++ extractFreeVars e2
extractFreeVars (Fst e) = extractFreeVars e
extractFreeVars (Snd e) = extractFreeVars e
extractFreeVars (Abs Discard e) = extractFreeVars e
extractFreeVars (Abs (V n) e) = filter (/= n) (extractFreeVars e)


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