module Player.Rooster (playerRooster) where

import System.Random (randomRIO)    
import Types
    
teamMembers :: String
teamMembers = "Rex L. and Alex Z."
    
playerRooster :: Player
playerRooster = Player getBetterRandomMove "Rooster"

getRandomMove :: Tile -> Board -> IO (Int, Int)
getRandomMove t b = do
    col <- randomRIO (1,dimM dim)
    row <- randomRIO (1,dimN dim)
    case b??(row, col) of
        EmptyTile -> return (row, col)
        _         -> getRandomMove t b

getBetterRandomMove :: Tile -> Board -> IO (Int, Int)
getBetterRandomMove t b 
    | movesSoFar `mod` 6 == 1 || movesSoFar `mod` 6 == 0 = return (head $ validMoves b)
    | otherwise = do
        idx <- randomRIO (0, (length positions) - 1)
        return (positions!!idx)
    where
        movesSoFar = numMoves b
        positions = getCloseMoves t b (dimN dim)

numMoves :: Board -> Int
numMoves [] = 0
numMoves ((_,t):b) 
    | t /= EmptyTile = 1 + numMoves b
    | otherwise      = numMoves b

getPositions :: Tile -> Board -> [Move] -> [Move]
getPositions tile [] a = a
getPositions tile ((m,ttt):tt) a 
    | tile == ttt = getPositions tile tt (m:a)
    | otherwise   = getPositions tile tt a

getFullBoard :: Int -> [Move]
getFullBoard n = [(x,y) | x <-[1..n], y<-[1..n]]

close :: [Move] -> Move -> Bool
close moves (x1,y1) = any (\(x,y) -> (abs (x1-x) + (abs (y1-y)) <= 2)) moves
         
getCloseMoves :: Tile -> Board -> Int -> [Move]
getCloseMoves tile b n =
    [m | m <- moves, b??m == EmptyTile, close used m]
    where 
        moves = getFullBoard n
        used  = getPositions tile b []
