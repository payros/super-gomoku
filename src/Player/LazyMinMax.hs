-- Team Member: Pedro Carvalho


module Player.LazyMinMax (playerLazyMinMax) where 

import Types
import Checks
import Prelude hiding ((!!))


playerLazyMinMax :: Player 
playerLazyMinMax = Player lazyMinMax "LazyMinMax"

lazyMinMax :: Tile -> Board -> IO Move
lazyMinMax tile board 
  = return $ snd $ maximum scoredMoves
  where
    scoredMoves = if (length lazyMoves) > 20 then (zip dumbScores moves) else (zip smartScores lazyMoves)
    dumbScores  = zipWith max (map (evaluateAttack tile . put board tile) moves) (map (evaluateDefense tile . put board (flipTile tile)) moves)
    smartScores = map (evaluateBoardMax 0 tile . put board tile) lazyMoves
    moves       = validMoves board
    lazyMoves   = filter (unisolatedMove board) moves

-- the score of board on tile, if such score exists, otherwise
-- the `minimum` score of `evaluateBoardMin` on the flipped tile, for all the valid moves of the flipped tile
evaluateBoardMax :: Int -> Tile -> Board -> Int
evaluateBoardMax depth tile board
  | depth > 2
  = 0
  | Just i <- scoreBoard tile board
  = i
  | otherwise
  = minimum (map (evaluateBoardMin (depth+1) (flipTile tile) . put board (flipTile tile)) moves)
  where
        moves       = filter (unisolatedMove board) (validMoves board)

-- the score of board on tile, if such score exists, otherwise
-- the `maximum` score of `evaluateBoardMax` on the flipped tile, for all the valid moves of the flipped tile
-- Also pass in the depth. If the depth is greater than the specified depth, return 0
evaluateBoardMin :: Int-> Tile -> Board -> Int
evaluateBoardMin depth tile board
  | depth > 2
  = 0
  | Just i <- scoreBoard (flipTile tile) board
  = i
  | otherwise
  = maximum (map (evaluateBoardMax (depth+1) (flipTile tile) . put board (flipTile tile)) moves)
  where
        moves       = filter (unisolatedMove board) (validMoves board)

-- Non-recursive Attack evaluation
evaluateAttack :: Tile -> Board -> Int
evaluateAttack tile board  
  | Just i <- scoreBoardR tile board
  =  i
  | otherwise
  = 0

-- Non-recursive Defense evaluation
evaluateDefense :: Tile -> Board -> Int
evaluateDefense tile board  
  | Just i <- scoreBoardR (flipTile tile) board
  =  (i*2)
  | otherwise
  = 0

-- Checks if a move is close to an edge or another move
unisolatedMove :: Board -> Move -> Bool
unisolatedMove board (x,y)
  | x == 1 || x == 15 || y == 1 || y == 15
  = True
  | board??(x-1,y-1) == EmptyTile && board??(x-1,y) == EmptyTile && board??(x-1,y+1) == EmptyTile && board??(x,y-1) == EmptyTile && board??(x,y+1) == EmptyTile && board??(x+1,y-1) == EmptyTile && board??(x+1,y) == EmptyTile && board??(x+1,y+1) == EmptyTile
  = False
  | otherwise
  = True

-- Restrictive scoreBoard
scoreBoardR :: Tile -> Board -> Maybe Int 
scoreBoardR tile board 
  | tileWins board tile 
  = Just 3
  | tile4 board tile
  = Just 2
  | tile3 board tile
  = Just 1
  | checkFull board           
  = Just 0
  | otherwise
  = Nothing

-- 3 tiles aligned
tile3 :: Board -> Tile -> Bool
tile3 b t = 
   any (\col -> any (\row -> all (\k -> b??(row,col+k)   == t) [0..3]) [1..dimM dim]) [1..dimN dim] ||
   any (\col -> any (\row -> all (\k -> b??(row+k,col)   == t) [0..3]) [1..dimM dim]) [1..dimN dim] ||
   any (\col -> any (\row -> all (\k -> b??(row+k,col+k) == t) [0..3]) [1..dimM dim]) [1..dimN dim] ||
   any (\col -> any (\row -> all (\k -> b??(row-k,col+k) == t) [0..3]) [1..dimM dim]) [1..dimN dim]

-- 4 tiles aligned
tile4 :: Board -> Tile -> Bool
tile4 b t = 
   any (\col -> any (\row -> all (\k -> b??(row,col+k)   == t) [0..4]) [1..dimM dim]) [1..dimN dim] ||
   any (\col -> any (\row -> all (\k -> b??(row+k,col)   == t) [0..4]) [1..dimM dim]) [1..dimN dim] ||
   any (\col -> any (\row -> all (\k -> b??(row+k,col+k) == t) [0..4]) [1..dimM dim]) [1..dimN dim] ||
   any (\col -> any (\row -> all (\k -> b??(row-k,col+k) == t) [0..4]) [1..dimM dim]) [1..dimN dim]