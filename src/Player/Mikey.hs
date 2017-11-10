module Player.Mikey (playerMikey) where

import Types
import Checks

import Data.List

import System.Random (randomRIO)

teamMembers :: String 
teamMembers = "Mikey"

playerMikey :: Player 
playerMikey = Player getMoveNearTheAction "Mikey"


-- Gets the average of all non empty tiles, and makes a move closest to those.
getMoveNearTheAction :: Tile -> Board -> Dimensions -> Int -> IO Move
getMoveNearTheAction tile board dim time
  | board == (emptyBoard dim) = getRandomMove tile (emptyBoard dim) dim
  | otherwise                 = return $ closestMove board $ aveNonEmptyMove (nonEmptyMoves tile board) dim


-- Takes a board and a move, and gets the closest move on the board that is empty.
closestMove :: Board -> Move -> Move
closestMove board (aveI, aveJ)
  | Just i <- elemIndex minDist distances = valids !! i
  | otherwise                             = head $ valids
  where
    valids    = validMoves board
    dist      = \(i, j) -> sqrt ((fromIntegral (aveI - i))^2 + (fromIntegral (aveJ - j))^2)
    distances = map dist valids
    minDist   = minimum distances
    index     = elemIndex minDist distances


-- Takes average i and j of list of moves
aveNonEmptyMove :: [Move] -> Dimensions -> Move
aveNonEmptyMove [] dim    = (dimN dim `div` 2, dimM dim `div` 2)
aveNonEmptyMove moves dim = sumTuple `moveDiv` numMoves
  where 
    sumTuple = foldr (\(i1,j1) (i2,j2) -> (i1+i2,j1+j2)) (0,0) moves
    numMoves = length moves

-- Divides both entries in a tuple of Ints by another Int.
moveDiv :: Move -> Int -> Move
moveDiv (i,j) n = (round (fromIntegral i / fromIntegral n), round (fromIntegral j / fromIntegral n))


nonEmptyMoves :: Tile -> Board -> [Move]
nonEmptyMoves tile board = [ij | (ij, t) <- board, t /= EmptyTile]


-- Copied from Computer.hs, gets a random valid move
getRandomMove :: Tile -> Board -> Dimensions -> IO (Int, Int)
getRandomMove t b dim = do
    col <- randomRIO (1,dimM dim)
    row <- randomRIO (1,dimN dim)
    case b??(row, col) of
        EmptyTile -> return (row, col)
        _         -> getRandomMove t b dim
