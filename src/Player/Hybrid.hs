module Player.Hybrid (playerHybrid, teamMembers) where 

import Types  
import Checks (scoreBoard)


teamMembers :: String
teamMembers = "John Kastner"

playerHybrid :: Player 
playerHybrid = Player strategy "Hybrid"

strategy :: Tile -> Board -> Dimensions -> Int -> IO Move
strategy tile board dim time = return $ snd $ maximum scoredMoves
  where
    scoredMoves = zip scores moves
    scores      = map (\m -> max (evaluateBoard tile (put board tile m) dim) (evaluateBoard (flipTile tile) (put board (flipTile tile) m) dim))  moves
    moves       = validMoves board


evaluateBoard :: Tile -> Board -> Dimensions -> Int
evaluateBoard tile board dim = max_in_row tile board dim

max_in_row t b dim = maximum [
   maximum $ map (\col -> maximum $ map (\row -> length $ takeWhile (\k -> b??(row,col+k)   == t) [0..dimK dim-1]) [1..dimM dim]) [1..dimN dim],
   maximum $ map (\col -> maximum $ map (\row -> length $ takeWhile (\k -> b??(row+k,col)   == t) [0..dimK dim-1]) [1..dimM dim]) [1..dimN dim],
   maximum $ map (\col -> maximum $ map (\row -> length $ takeWhile (\k -> b??(row+k,col+k) == t) [0..dimK dim-1]) [1..dimM dim]) [1..dimN dim],
   maximum $ map (\col -> maximum $ map (\row -> length $ takeWhile (\k -> b??(row-k,col+k) == t) [0..dimK dim-1]) [1..dimM dim]) [1..dimN dim]]
