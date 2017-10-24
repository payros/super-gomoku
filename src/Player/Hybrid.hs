module Player.Hybrid (playerHybrid, teamMembers) where 

import Types  
import Checks (scoreBoard)


teamMembers :: String
teamMembers = "John Kastner"

playerHybrid :: Player 
playerHybrid = Player strategy "Hybrid"

strategy :: Tile -> Board -> IO Move
strategy tile board = return $ snd $ maximum scoredMoves 
  where
    scoredMoves = zip scores moves
    scores      = map (\m -> max (evaluateBoard tile . put board tile $ m) (evaluateBoard (flipTile tile) . put board (flipTile tile) $ m))  moves 
    moves       = validMoves board


evaluateBoard :: Tile -> Board -> Int
evaluateBoard tile board = max_in_row tile board

max_in_row t b = maximum [
   maximum $ map (\col -> maximum $ map (\row -> length $ takeWhile (\k -> b??(row,col+k)   == t) [0..dimK dim-1]) [1..dimM dim]) [1..dimN dim],
   maximum $ map (\col -> maximum $ map (\row -> length $ takeWhile (\k -> b??(row+k,col)   == t) [0..dimK dim-1]) [1..dimM dim]) [1..dimN dim],
   maximum $ map (\col -> maximum $ map (\row -> length $ takeWhile (\k -> b??(row+k,col+k) == t) [0..dimK dim-1]) [1..dimM dim]) [1..dimN dim],
   maximum $ map (\col -> maximum $ map (\row -> length $ takeWhile (\k -> b??(row-k,col+k) == t) [0..dimK dim-1]) [1..dimM dim]) [1..dimN dim]]
