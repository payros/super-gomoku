-- Chooses random element from validMoves

module Player.HeadlessChicken (playerHeadlessChicken) where

import System.Random (randomRIO)

import Types
import Data.List

teamMembers :: String
teamMembers = "Andrew Putlock and Brian Oluwo"

playerHeadlessChicken :: Player
playerHeadlessChicken = Player strategy teamMembers

strategy :: Tile -> Board -> Dimensions -> Int -> IO (Int, Int)
strategy t b dim time = do
  x <- randomRIO (0, length safeMoves - 1)
  y <- randomRIO (0, (length $ closestMove b $ safeMoves !! x) - 1)
  return $ (closestMove b $ safeMoves !! x) !! y
  where
    safeMoves = if length enemyMoves == 0 then [head $ validMoves b] else enemyMoves
    enemyMoves = movesByTile (flipTile t) b

movesByTile :: Tile -> Board -> [Move]
movesByTile t b = foldl' (\acc (pos, tl) -> if t == tl then pos:acc else acc) [] b

closestMove :: Board -> Move -> [Move]
closestMove b m = map snd filtered
  where
    filtered = filter (\(x, _) -> x == closest) distMvs
    closest = fst $ head distMvs
    distMvs = sort $ map (\mv -> (distance m mv, mv)) mvs
    mvs = validMoves b

distance :: Move -> Move -> Int
distance (x1, y1) (x2, y2) = max (abs (x1 - x2)) (abs (y1 - y2))
