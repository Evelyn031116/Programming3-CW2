{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ParallelListComp #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
-- Jingyi Zhong comp2209 Functional Programming Challenges
-- jz14g22@soton.ac.uk
-- (c) University of Southampton 2023
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

-- Challenge 1
-- Testing Circuits

data TileEdge = North | East | South | West  deriving (Eq,Ord,Show,Read)
data Tile = Source [ TileEdge ] | Sink [ TileEdge ] | Wire [ TileEdge ]  deriving (Eq,Show,Read)
type Puzzle = [ [ Tile ] ]
type TileCoords = (Int, Int)

-- Check if the puzzle is complete by ensuring wires are connected, sources link to sinks and vice versa.
isPuzzleComplete :: Puzzle -> Bool
isPuzzleComplete puzzle = areWiresConnected puzzle 
                       && sourcesLinkedSinks puzzle 
                       && sinksLinkedSources puzzle

-- Check if all wires in the puzzle are connected.
areWiresConnected :: Puzzle -> Bool
areWiresConnected puzzle = 
  all (\coords -> all (isConnected coords) (getNeighborCoords coords)) wireCoords
  where
    -- List all coordinates of wire tiles in the puzzle.
    wireCoords :: [TileCoords]
    wireCoords = [(i,j) | i <- [0..length puzzle - 1], j <- [0..length (head puzzle) - 1], isWire (puzzle !! i !! j)]

    -- Get valid neighboring coordinates of a given tile.
    getNeighborCoords :: TileCoords -> [TileCoords]
    getNeighborCoords (x, y) = 
      filter isCoordValid [(x - 1, y), (x, y - 1), (x, y - 1), (x, y + 1)]

    -- Check if the coordinates are within the puzzle boundaries.
    isCoordValid :: TileCoords -> Bool
    isCoordValid (x, y) = x >= 0 && y >= 0 && x < length puzzle && y < length (head puzzle)

    -- Check if two tiles are connected.
    isConnected :: TileCoords -> TileCoords -> Bool
    isConnected coord1 coord2
      = connectedEdge
          coord1
          coord2
          (getEdges (puzzle !! fst coord1 !! snd coord1))
          (getEdges (puzzle !! fst coord2 !! snd coord2))
        && notConnectedTo coord1
        && notConnectedTo coord2

    -- Check if the edges of two tiles are connected based on their positions.
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

    -- Ensure the tile is not incorrectly connected to the puzzle boundary.
    notConnectedTo :: TileCoords -> Bool
    notConnectedTo (x, y)
      | x == 0 && North `elem` getEdges (puzzle !! x !! y) = False
      | x == length puzzle - 1 && South `elem` getEdges (puzzle !! x !! y) = False
      | y == 0 && West `elem` getEdges (puzzle !! x !! y) = False
      | y == length (head puzzle) - 1 && East `elem` getEdges (puzzle !! x !! y) = False
      | otherwise = True

-- Check if all sources are linked to sinks in the puzzle.
sourcesLinkedSinks :: Puzzle -> Bool
sourcesLinkedSinks puzzle
 | null sources = False
 | otherwise = all (\sink -> any (\source -> hasPath puzzle sink source) sources) sinks
   where
     sources = [(i, j) | i <- [0..length puzzle - 1], j <- [0..length (head puzzle) - 1], isSource (puzzle !! i !! j)]
     sinks= [(i, j) | i <- [0..length puzzle - 1], j <- [0..length (head puzzle) - 1], isSink (puzzle !! i !! j)]

-- Check if all sinks are linked to sources in the puzzle.
sinksLinkedSources :: Puzzle -> Bool
sinksLinkedSources puzzle1
  | null sinks = False
  | otherwise = all (\source -> any (\sink -> hasPath puzzle1 source sink) sinks) sources
    where
      sources = [(i, j) | i <- [0..length puzzle1 - 1], j <- [0..length (head puzzle1) - 1], isSource (puzzle1 !! i !! j)]
      sinks= [(i, j) | i <- [0..length puzzle1 - 1], j <- [0..length (head puzzle1) - 1], isSink (puzzle1 !! i !! j)]

-- Get the edges of a tile.
getEdges :: Tile -> [TileEdge]
getEdges (Source edges) = edges
getEdges (Sink edges) = edges
getEdges (Wire edges) = edges

-- Check if a tile is a wire.
isWire :: Tile -> Bool
isWire (Wire _) = True
isWire _ = False

-- Check if a tile is a source.
isSource :: Tile -> Bool
isSource (Source _) = True
isSource _ = False

-- Check if a tile is a sink.
isSink :: Tile -> Bool
isSink (Sink _) = True
isSink _ = False

-- This function checks if there's a path between two coordinates in the puzzle.
hasPath :: Puzzle -> TileCoords -> TileCoords -> Bool
hasPath puzzle coordsHead coordsTail = dfs coordsHead coordsTail []
  where
    dfs :: TileCoords -> TileCoords -> [TileCoords] -> Bool
    dfs currentCoord targetCoord visited
      | currentCoord == targetCoord = True
      | currentCoord `elem` visited = False
      | otherwise = any (\candidate -> dfs candidate targetCoord (currentCoord : visited)) nextCoords
        where
          -- nextCoords are the adjacent coordinates to the current one that have not been visited yet.
          nextCoords :: [TileCoords]
          nextCoords = filter (`notElem` visited) (getNextCoords currentCoord)
          -- The getNextCoords function calculates the adjacent coordinates based on the edges of the current tile.
          getNextCoords :: TileCoords -> [TileCoords]
          getNextCoords (x, y) =
              let maxX = length puzzle - 1
                  maxY = length (head puzzle) - 1
                  validCoord (a, b) = a >= 0 && a <= maxX && b >= 0 && b <= maxY
                  edges = if x >= 0 && x <= maxX && y >= 0 && y <= maxY
                          then getEdges (puzzle !! x !! y)
                          else []
              -- Filter out valid adjacent coordinates based on the edges of the current tile.
              in filter validCoord
               [(x - 1, y) | North `elem` edges] ++
               [(x + 1, y) | South `elem` edges] ++
               [(x, y - 1) | West `elem` edges] ++
               [(x, y + 1) | East `elem` edges]

-- Challenge 2
-- Solving Circuits
data Rotation = R0 | R90 | R180 | R270
 deriving (Eq,Show,Read)

-- Solve the circuit puzzle, returning a solution as a list of lists of Rotations, if one exists.
solveCircuit :: Puzzle -> Maybe [[Rotation]]
solveCircuit puzzle = backtrack puzzle (0, 0) (initialRotations puzzle) [] 

-- Initialize the rotations for all tiles in the puzzle to R0.
initialRotations :: Puzzle -> [[Rotation]]
initialRotations puzzle = replicate (length puzzle) (replicate (length (head puzzle)) R0)

-- Define how each TileEdge is affected by a given Rotation.
rotateEdge :: Rotation -> TileEdge -> TileEdge
-- R90
rotateEdge R90 North = East
rotateEdge R90 East = South
rotateEdge R90 South = West
rotateEdge R90 West = North
-- R180
rotateEdge R180 North = South
rotateEdge R180 East = West
rotateEdge R180 South = North
rotateEdge R180 West = East
-- R270
rotateEdge R270 North = West
rotateEdge R270 East = North
rotateEdge R270 South = East
rotateEdge R270 West = South
rotateEdge _ edge = edge

-- Apply a given Rotation to a Tile, affecting its edges.
transferToRotateTile :: Rotation -> Tile -> Tile
transferToRotateTile R0 t = t
transferToRotateTile R90 (Wire wires) = Wire (map (rotateEdge R90) wires)
transferToRotateTile R90 (Source sources) = Source (map (rotateEdge R90) sources)
transferToRotateTile R90 (Sink sinks) = Sink (map (rotateEdge R90) sinks)
transferToRotateTile R180 (Wire wires) = Wire (map (rotateEdge R180) wires)
transferToRotateTile R180 (Source sources) = Source (map (rotateEdge R180) sources)
transferToRotateTile R180 (Sink sinks) = Sink (map (rotateEdge R180) sinks)
transferToRotateTile R270 (Wire wires) = Wire (map (rotateEdge R270) wires)
transferToRotateTile R270 (Source sources) = Source (map (rotateEdge R270) sources)
transferToRotateTile R270 (Sink sinks) = Sink (map (rotateEdge R270) sinks)

-- Rotate the entire puzzle based on the given rotations.
rotatePuzzle :: [[Rotation]] -> Puzzle -> Puzzle
rotatePuzzle rotations previous = zipWith (zipWith (flip transferToRotateTile)) previous rotations
  where
    partitionRotate :: [Tile] -> [Rotation] -> [Tile]
    partitionRotate tiles rotations = zipWith transferToRotateTile rotations tiles

-- Check if the solution for a specific tile (at coordinates i, j) in the puzzle is correct.
isCorrectAnswer :: Puzzle -> (Int, Int) -> [[Rotation]] -> Bool
isCorrectAnswer puzzle (i, j) rotations
  =  isConnectedToNeighbours puzzle i j rotations
  && notConnectedToPuzzleEdge
    where
      -- Check if the tile is not incorrectly connected to the puzzle's edge.
      notConnectedToPuzzleEdge :: Bool
      notConnectedToPuzzleEdge
        -- If the tile is on the top row and its North edge is active, it's incorrectly connected to the puzzle edge.
        | i == 0 && North `elem` rotateAndGetEdges puzzle rotations (i, j) = False
        -- Same logic as above
        | i == length puzzle - 1 && South `elem` rotateAndGetEdges puzzle rotations (i, j) = False
        | j == 0 && West `elem` rotateAndGetEdges puzzle rotations (i, j) = False
        | j == length (head puzzle) - 1 && East `elem` rotateAndGetEdges puzzle rotations (i, j) = False
        | otherwise = True

      -- Check if the tile is correctly connected to its neighbouring tiles both vertically (up and down) and horizontally (left and right).
      isConnectedToNeighbours :: Puzzle -> Int -> Int -> [[Rotation]] -> Bool
      isConnectedToNeighbours puzzle i j rotations
        =  isConnectedUpAndDown    puzzle i j rotations
        && isConnectedLeftAndRight puzzle i j rotations
        where
          -- Check if the tile is correctly connected to its upper and lower neighbours.
          isConnectedUpAndDown :: Puzzle -> Int -> Int -> [[Rotation]] -> Bool
          isConnectedUpAndDown puzzle i j rotations
            -- If the tile is on the top row, it cannot be incorrectly connected upwards.
            | i == 0 = True
            -- Same logic as above
            | South `elem` rotateAndGetEdges puzzle rotations (i - 1, j)
                        && North `elem` rotateAndGetEdges puzzle rotations (i, j) = True
            | South `notElem` rotateAndGetEdges puzzle rotations (i - 1, j)
                        && North `notElem` rotateAndGetEdges puzzle rotations (i, j) = True
            | otherwise = False

          -- Similar to isConnectedUpAndDown.
          isConnectedLeftAndRight :: Puzzle -> Int -> Int -> [[Rotation]] -> Bool
          isConnectedLeftAndRight puzzle i j rotations
            | j == 0 = True
            | East `elem` rotateAndGetEdges puzzle rotations (i, j - 1)
                        && West `elem` rotateAndGetEdges puzzle rotations (i, j) = True
            | East `notElem` rotateAndGetEdges puzzle rotations (i, j - 1)
                        && West `notElem` rotateAndGetEdges puzzle rotations (i, j) = True
            | otherwise = False

-- Get the edges of a tile after applying the given rotation.
rotateAndGetEdges :: Puzzle -> [[Rotation]] -> (Int, Int) -> [TileEdge]
rotateAndGetEdges puzzle rotations (i, j) = getEdges $ transferToRotateTile (rotations !! i !! j) (puzzle !! i !! j)

-- The backtrack function attempts to solve the puzzle by recursively trying different rotations for each tile.
-- It either returns a solution as a list of rotations for each tile or Nothing if no solution exists.
backtrack :: Puzzle -> (Int, Int) -> [[Rotation]] -> [((Int, Int), Rotation)]    -> Maybe [[Rotation]]
backtrack puzzle (i, j) rotations nextList 
  -- Check if the end of the puzzle has been reached and if the puzzle is complete.
  | isEnd (i, j) puzzle && isPuzzleComplete (rotatePuzzle rotations puzzle)= Just rotations
  -- Try rotation R0, R90, R180 and R270 one by one for the current tile, and if the result is correct, proceed to the next tile.
  -- Then save the next node of the same layer to nextList for easy backtracking.

  | isCorrectAnswer puzzle (i, j) (addCorrectAnswer (i, j) rotations R0) =
      backtrack puzzle (nextTile (i, j) puzzle) (addCorrectAnswer (i, j) rotations R0) (((i, j), R90) : nextList) 
  | isCorrectAnswer puzzle (i, j) (addCorrectAnswer (i, j) rotations R90) =
      backtrack puzzle (nextTile (i, j) puzzle) (addCorrectAnswer (i, j) rotations R90) (((i, j), R180) : nextList) 
  | isCorrectAnswer puzzle (i, j) (addCorrectAnswer (i, j) rotations R180) =
      backtrack puzzle (nextTile (i, j) puzzle) (addCorrectAnswer (i, j) rotations R180) (((i, j), R270) : nextList) 
  | isCorrectAnswer puzzle (i, j) (addCorrectAnswer (i, j) rotations R270) =
      backtrack puzzle (nextTile (i, j) puzzle) (addCorrectAnswer (i, j) rotations R270) nextList 
  | null nextList = Nothing
  -- If all rotations for the current tile lead to an incorrect answer, backtrack to the previous tile and try the next rotation.
  | otherwise =
    backtrack puzzle (fst (head nextList)) (deleteWrongAnswerAndAddNewAnswer rotations (head nextList)) (drop 1 nextList) 

  where
    checkBt :: Rotation -> Rotation -> Bool
    checkBt currr btr
      | btr == R90  && currr == R90  = True
      | btr == R90  && currr == R180 = True
      | btr == R90  && currr == R270 = True
      | btr == R180 && currr == R270 = True
      | btr == R180 && currr == R270 = True
      | btr == R270 && currr == R270 = True
      | otherwise = False
    -- Add the correct rotation to the puzzle for a given tile.
    addCorrectAnswer :: (Int, Int) -> [[Rotation]] -> Rotation -> [[Rotation]]
    addCorrectAnswer (i, j) rotations newRotation =
        -- Replace the current rotation of the tile at position (i, j) with the new rotation.
        take i rotations ++ [updateRow (rotations !! i) j newRotation] ++ drop (i + 1) rotations
        where
            -- Update a specific row of the puzzle with the new rotation at a given column.
            updateRow :: [Rotation] -> Int -> Rotation -> [Rotation]
            updateRow row j newRotation =
                -- Insert the new rotation into the specified position in the row.
                take j row ++ [newRotation] ++ drop (j + 1) row

    -- Delete the incorrect rotation from the puzzle and replace it with a new rotation.
    deleteWrongAnswerAndAddNewAnswer :: [[Rotation]] -> ((Int, Int), Rotation) -> [[Rotation]]
    deleteWrongAnswerAndAddNewAnswer rotations ((i, j), r) =
        let rows = length rotations
            cols = length (head rotations)
            -- Create a new row with the corrected rotation for the tile and reset the remaining tiles in the row.
            newRow = take j (rotations !! i) ++ [r] ++ replicate (cols - j - 1) R0
        -- Replace the incorrect row with the new row and reset the subsequent rows.
        in take i rotations ++ [newRow] ++ replicate (rows - i - 1) (replicate cols R0)

    -- Determine the next tile to consider during the backtracking process.
    nextTile :: (Int, Int) -> Puzzle -> (Int, Int)
    nextTile (i, j) puzzle
      -- Move to the next tile in the same row, or to the first tile in the next row if at the end of a row.
      | j < width - 1 = (i, j + 1)
      | i < height - 1 = (i + 1, 0)
      -- Remain at the same tile if it's the last one in the puzzle.
      | otherwise = (i, j)
        where
          height = length puzzle
          width = length (head puzzle)

    -- Check if the current position is the end of the puzzle.
    isEnd :: (Int, Int) -> Puzzle -> Bool
    isEnd (i, j) puzzle
        | i == length puzzle - 1 && j == length (head puzzle)-1  = True
        | otherwise = False

-- Challenge 3
-- Pretty Printing Let Expressions

data LExpr = Var Int | App LExpr LExpr | Let Bind  LExpr LExpr | Pair LExpr LExpr | Fst LExpr | Snd LExpr  | Abs Bind LExpr
    deriving (Eq,Show,Read)
data Bind = Discard | V Int
    deriving (Eq,Show,Read)

-- Cases for pretty-printing each type of lambda expression.
prettyPrint :: LExpr -> String
prettyPrint (Var i) = "x" ++ show i
prettyPrint (App e1 e2) = handleLeftApp e1 ++ " " ++ handleRightApp e2
prettyPrint (Let b e1 e2) = "let " ++ handleLetBindings b e1 ++ "in " ++ prettyPrint e2 
  where
    -- Handle let bindings for nested abstractions. 
    handleLetBindings :: Bind -> LExpr -> String
    handleLetBindings bind (Abs b1 e1') = prettyBind bind ++ " " ++ handleLetBindings b1 e1'
    handleLetBindings bind e1' = prettyBind bind ++ " = " ++ prettyPrint e1' ++ " "

prettyPrint (Fst e) = "fst (" ++ prettyPrint e ++ ")"
prettyPrint (Snd e) = "snd (" ++ prettyPrint e ++ ")"
prettyPrint (Abs b e) = "\\" ++ handleAbsBindings b e
  where
    -- Handle nested abstractions.
    handleAbsBindings :: Bind -> LExpr -> String
    handleAbsBindings bind (Abs b1 e1') = prettyBind bind ++ " " ++ handleAbsBindings b1 e1'
    handleAbsBindings bind e1' = prettyBind bind ++ " -> " ++ prettyPrint e1'
prettyPrint (Pair e1 e2) = "(" ++ prettyPrint e1 ++ ", " ++ prettyPrint e2 ++ ")"

-- Helper function for handling let-bindings in pretty-printing.
handleLetBindings :: Bind -> LExpr -> String
handleLetBindings bind (Abs b1 e1') = prettyBind bind ++ " " ++ handleLetBindings b1 e1'
handleLetBindings bind e1' = prettyBind bind ++ " = " ++ prettyPrint e1' ++ " "

-- Helper function for pretty-printing binders.
prettyBind :: Bind -> String
prettyBind Discard = "_"
prettyBind (V i) = "x" ++ show i

-- Helper function for handling abstractions in pretty-printing.
handleAbs :: LExpr -> String
handleAbs (Abs b e) = prettyBind b ++ " " ++ handleAbs e
handleAbs (Let b e1 e2) = "= " ++ prettyPrint (Let b e1 e2)
handleAbs e = "= " ++ prettyPrint e

prettyPrintInner :: LExpr -> String
prettyPrintInner (Abs b inner) = case b of
  Discard -> " _" ++ prettyPrintInner inner
  V i     -> " " ++ prettyBind b ++ prettyPrintInner inner
prettyPrintInner e = " -> " ++ prettyPrint e

-- Helper function for handling left associative application in pretty-printing.
handleLeftApp :: LExpr -> String
handleLeftApp (App e1 e2) = handleLeftApp e1 ++ " " ++ handleLeftApp e2
handleLeftApp (Var n) = prettyPrint (Var n)
handleLeftApp (Let b e1 e2) = "(let " ++ handleLetBindings b e1 ++ "in " ++ handleLeftApp e2 ++ ")"
  where
    handleLetBindings bind (Abs b1 e1') = prettyBind bind ++ " " ++ handleLetBindings b1 e1'
    handleLetBindings bind e1' = prettyBind bind ++ " = " ++ prettyPrint e1' ++ " "
handleLeftApp (Fst e) = prettyPrint (Fst e)
handleLeftApp (Snd e) = prettyPrint (Snd e)
handleLeftApp (Pair e1 e2) = prettyPrint (Pair e1 e2)
handleLeftApp e = "(" ++ prettyPrint e ++ ")"

-- Helper function for handling right associative application in pretty-printing.
handleRightApp :: LExpr -> String
handleRightApp (App e1 e2) = "(" ++ handleLeftApp e1 ++ " " ++ handleRightApp e2 ++ ")"
handleRightApp e = prettyPrint e

-- Challenge 4 - Parsing Let Expressions

-- Main function to parse a string into a lambda expression (LExpr).
-- It first checks for correct spacing and then tries to parse the expression.
parseLetx :: String -> Maybe LExpr
parseLetx input | handleSpace input == False =Nothing
                | otherwise = case parse parseLExpr input of
    [(result, "")] -> Just result
    _              -> Nothing

-- Parser for different types of lambda expressions.
parseLExpr :: Parser LExpr
parseLExpr = parseLet <|> parseAbs <|> parseApp <|> parsePair <|> parseVar <|> parseFst <|> parseSnd <|> handleApp

-- Parser for bindings in lambda expressions.
parseBind :: Parser Bind
parseBind = do
  parseV <|> parseDiscard
  where
    parseV = do
      space
      char 'x'
      n <- nat
      space
      return (V n)

    parseDiscard = do
      symbol "_"
      return (Discard)

-- Parser for variable binding.
parseVar :: Parser LExpr
parseVar = do
  space
  char 'x'
  n <- nat
  space
  return (Var n)

-- Parser for lambda abstraction.
parseAbs :: Parser LExpr
parseAbs = do
    symbol "\\"
    b <- some parseBind
    symbol "->"
    e <- parseLExpr
    return (foldr Abs e b)

-- Parser for application expressions.
removeBrackets :: Parser LExpr -> Parser LExpr
removeBrackets parser = do
  symbol "("
  e <- parser
  symbol ")"
  return e

-- Handle different types of expressions inside an application.
parseApp :: Parser LExpr
parseApp = do
  es <- some handleApp
  return (foldl1 App es)

-- Parser for 'let' expressions.
handleApp :: Parser LExpr
handleApp = removeBrackets parseLExpr <|> parseVar <|> parseFst <|> parseSnd <|> parseLet <|> parseAbs

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

-- Function to fold bindings into an expression.
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
  symbol "("
  e1 <- parseLExpr
  symbol ","
  e2 <- parseLExpr
  symbol ")"
  symbol ")"
  return (Fst (Pair e1 e2))

parseSnd :: Parser LExpr
parseSnd = do
  symbol "snd"
  symbol "("
  symbol "("
  e1 <- parseLExpr
  symbol ","
  e2 <- parseLExpr
  symbol ")"
  symbol ")"
  return (Snd (Pair e1 e2))

-- Helper function for handling spaces in the input string.
-- It ensures that digits are not directly followed by 'x', which would be a syntax error.
handleSpace :: String -> Bool
handleSpace [] = True
handleSpace (x : xs)
  | isDigit x = case xs of
    ('x' : _) -> False
    _         -> handleSpace xs
  | otherwise = handleSpace xs

-- Challenge 5
-- Let Encoding in Lambda 

data LamExpr = LamVar Int | LamApp LamExpr LamExpr | LamAbs Int LamExpr
                deriving (Eq, Show, Read)

-- Function to encode LExpr into LamExpr, handling different types of LExpr.
letEnc :: LExpr -> LamExpr
letEnc (Var n) = LamVar n
letEnc (App e1 e2) = LamApp (letEnc e1) (letEnc e2)
letEnc (Let Discard e1 e2) = let fresh = findNextUnused (extractFreeVars e2) in LamApp (LamAbs fresh (letEnc e2)) (letEnc e1)
letEnc (Let (V n) e1 e2) = LamApp (LamAbs n (letEnc e2)) (letEnc e1)
letEnc (Pair e1 e2) = let fresh = findNextUnused (extractFreeVars e1 ++ extractFreeVars e2) in LamAbs fresh (LamApp (LamApp (LamVar fresh) (letEnc e1)) (letEnc e2))
letEnc (Abs Discard e) = let fresh = findNextUnused (extractFreeVars e) in LamAbs fresh (letEnc e)
letEnc (Abs (V n) e) = LamAbs n (letEnc e)
letEnc (Fst e) = LamApp (letEnc e) (LamAbs 0 (LamAbs 1 (LamVar 0)))
letEnc (Snd e) = LamApp (letEnc e) (LamAbs 0 (LamAbs 1 (LamVar 1)))

-- Finds the next unused variable identifier.
findNextUnused :: [Int] -> Int
findNextUnused usedVars = head $ filter (`notElem` usedVars) [2..]

-- Extracts free variables from a given LExpr.
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
rename x e | free (x + 1) e = rename (x + 1) e
           | otherwise = x + 1

subst :: LamExpr -> Int ->  LamExpr -> LamExpr
subst (LamVar x) y e | x == y = e
subst (LamVar x) y e | x /= y = LamVar x
subst (LamAbs x e1) y e  |  x /= y && not (free x e)  = LamAbs x (subst e1 y e)
subst (LamAbs x e1) y e  |  x /= y &&     (free x e)  = let x' = (rename x e1) in subst (LamAbs x' (subst e1 x (LamVar x'))) y e
subst (LamAbs x e1) y e  |  x == y  = LamAbs x e1
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
-- Substitutes a variable with an expression in a lambda expression.
substLExpr :: LExpr -> Int -> LExpr -> LExpr
substLExpr (Var x) y e | x == y = e
substLExpr (Var x) y e | x /= y = Var x
substLExpr (App e1 e2) y e = App (substLExpr e1 y e) (substLExpr e2 y e)
substLExpr (Abs bind e1) y e = Abs bind (substLExpr e1 y e)
substLExpr (Let bind e1 e2) y e = Let bind (substLExpr e1 y e) (substLExpr e2 y e)
substLExpr (Let Discard e1 e2) _ _ = e2
substLExpr (Pair e1 e2) y e = Pair (substLExpr e1 y e) (substLExpr e2 y e)
substLExpr (Fst e1) y e = Fst (substLExpr e1 y e)
substLExpr (Snd e1) y e = Snd (substLExpr e1 y e)

-- Determines if a given lambda expression is a value
isLExprValue :: LExpr -> Bool
isLExprValue expr = case cbvLet expr of
    Nothing -> True
    Just _  -> False

-- Call-by-value reduction for lambda expressions.
cbvLet :: LExpr -> Maybe LExpr
cbvLet (App (Abs bind e1) e2)
   -- If the body of the abstraction is not a value, reduce it first.
  | not (isLExprValue e1) =
    do
      e' <- cbvLet e1
      return (App (Abs bind e') e2)
  -- If the argument of the abstraction is not a value, reduce it.
  | not (isLExprValue e2) =
    do
      e' <- cbvLet e2
      return (App (Abs bind e1) e')
  | isLExprValue e1 && isLExprValue e2 = case bind of
          V n -> return (substLExpr e1 n e2) -- Substitute the bound expression in the body.
          Discard -> return e1 -- Discard the bound expression if the binder is '_'.

-- Same logic as above.
cbvLet (Let bind e1 e2)
  | not (isLExprValue e1) =
    do
      e' <- cbvLet e1
      return (Let bind e' e2)
  | not (isLExprValue e2) =
    do
      e' <- cbvLet e2
      return (Let bind e1 e')
  | isLExprValue e1 && isLExprValue e2 = case bind of
          V n -> return (substLExpr e2 n e1)
          Discard -> return e1

cbvLet (Pair e1 e2)
  | not (isLExprValue e1) =
    do
      e' <- cbvLet e1
      return (Pair e' e2)
  | not (isLExprValue e2) =
    do
      e' <- cbvLet e2
      return (Pair e1 e')

cbvLet (Fst (Pair e1 e2))
  | not (isLExprValue e1) =
    do
      e' <- cbvLet e1
      return (Fst (Pair e' e2))
  | not (isLExprValue e2) =
    do
      e' <- cbvLet e2
      return (Fst (Pair e1 e'))
  | isLExprValue e1 && isLExprValue e2 =
      return e1

cbvLet (Snd (Pair e1 e2))
  | not (isLExprValue e1) =
    do
      e' <- cbvLet e1
      return (Snd (Pair e' e2))
  | not (isLExprValue e2) =
    do
      e' <- cbvLet e2
      return (Snd (Pair e1 e'))
  | isLExprValue e1 && isLExprValue e2 =
      return e2

cbvLet _ = Nothing

-- Call-by-name reduction for lambda expressions.
cbnLet :: LExpr -> Maybe LExpr
cbnLet (App (Abs Discard e1) e2) = Just e1
cbnLet (App (Abs b e1) e2) = Just (substLExpr e1 (bindToInt b) e2)
  where
     bindToInt :: Bind -> Int
     bindToInt (V n) = n
cbnLet (Let (V n) e1 e2) = Just (substLExpr e2 n e1)
cbnLet (Let Discard e1 e2) = Just e2
cbnLet (Pair e1 e2) = Just e1
cbnLet (Fst (Pair e1 e2)) = Just e1
cbnLet (Snd (Pair e1 e2)) = Just e2
cbnLet _ = Nothing

-- Count the number of reductions required to fully reduce an expression.
countLamReductions :: (LamExpr -> Maybe LamExpr) -> LamExpr -> Int -> Int
countLamReductions reduce expr limit = go 0 expr
  where
    go n e | n >= limit = limit
           | otherwise = case reduce e of
                           Just e' -> go (n + 1) e'
                           Nothing -> n

countLetReductions :: (LExpr -> Maybe LExpr) -> LExpr -> Int -> Int
countLetReductions reduce expr limit = go 0 expr
  where
    go n e | n >= limit = limit
           | otherwise = case reduce e of
                           Just e' -> go (n + 1) e'
                           Nothing -> n

-- Function to compare reduction strategies and return their respective reduction counts.
compareRedn :: LExpr -> Int -> (Int, Int, Int, Int)
compareRedn expr limit =
  ( countLetReductions cbvLet expr limit
  , countLamReductions cbvlam1 (letEnc expr) limit
  , countLetReductions cbnLet expr limit
  , countLamReductions cbnlam1 (letEnc expr) limit
  )
