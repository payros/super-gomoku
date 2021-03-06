module Player.Notemotives(playerNotemotives) where

import Types  --(Player(..), Tile(EmptyTile), Board, Move, closeMoves, put, flipTile,(??))
import Checks (scoreBoard, checkFull)
import Data.Maybe
import System.Random (randomRIO)
import Debug.Trace(trace)
import Data.Bool

teamMembers :: String 
teamMembers = "Notemotives"

playerNotemotives :: Player 
playerNotemotives = Player computeMove "Notemotives"

------------------------------------------------closeMoves------------------------------------------------------
----------------------------------------------------------------------------------------------------------------
closeMoves :: Board -> [Move]
closeMoves board  = giveBack board c
        where
                c = [ij | (ij, EmptyTile) <- board]

giveBack :: Board -> [Move] -> [Move]
giveBack board [] = []
giveBack board (b:bs) 
        | evaluate board b == True = b:giveBack board bs
        | otherwise = giveBack board bs

evaluate :: Board -> Move -> Bool
evaluate board (a,b) 
        | board??(a-1,b+1) /= EmptyTile = True
        | board??(a-1,b) /= EmptyTile = True
        | board??(a-1,b-1) /= EmptyTile = True
        | board??(a+1,b+1) /= EmptyTile = True
        | board??(a+1,b) /= EmptyTile = True
        | board??(a+1,b-1) /= EmptyTile = True
        | board??(a,b+1) /= EmptyTile = True
        | board??(a,b-1) /= EmptyTile = True
        | otherwise = False

{-
-- Modifies old closeMoves list updating it with the new move
increm_closeMoves :: [Move] -> Board -> Move -> [Move]
increm_closeMoves oldMoves oldBoard move newMoves = 
  where oldMoves' = delete oldMoves move


delete :: Eq a => a -> [a] -> [a]
delete deleted xs = [ x | x <- xs, x /= deleted ]
-}

------------------------------------------------Minimax---------------------------------------------------------
----------------------------------------------------------------------------------------------------------------
maxDepth :: Int
maxDepth = 2

debug :: Bool
debug = False

computeMove :: Tile -> Board -> Dimensions -> Int -> IO Move
computeMove tile board dim time
  | moves == []
  = getRandomMove dim
  | otherwise
  = return $ snd $ maximum scoredMoves 
  where
        scoredMoves = zip scores moves
        scores      = [evaluateBoardMax (maxDepth-1) x tile (put board tile x) dim | x <- moves]
        --moves       = trace ("mo: " ++ show (closeMoves board)) $ closeMoves board
        moves       = closeMoves board

evaluateBoardMax :: Int -> Move -> Tile -> Board -> Dimensions -> Int
evaluateBoardMax depth lastMove tile board dim
  | isJust score
  -- = trace ("score: " ++ show (fromJust score)) $ fromJust score
  = fromJust score
  | depth == 0 || moves == []
  -- = trace ("s: " ++ show ((evaluateScore board dim tile) - (evaluateScore board dim (flipTile tile)))) $ (evaluateScore board dim tile) - (evaluateScore board dim (flipTile tile))
  = (evaluateScore board dim tile) - (evaluateScore board dim (flipTile tile)) `div` 2
  | otherwise
  --  trace ("p: " ++ show (minimum scores)) $ minimum scores
  = minimum scores
  where
        score       = quickScoreBoard tile board dim lastMove
        scores      = [evaluateBoardMin (depth-1) x (flipTile tile) (put board (flipTile tile) x) dim | x <- moves]
        moves       = closeMoves board

evaluateBoardMin :: Int -> Move -> Tile -> Board -> Dimensions -> Int
evaluateBoardMin depth lastMove tile board dim
  | isJust score
  = - (fromJust score)
  | depth == 0 || moves == []
  = (evaluateScore board dim (flipTile tile)) - (evaluateScore board dim tile) `div` 2
  | otherwise
  = maximum scores 
  where
        score       = quickScoreBoard tile board dim lastMove
        scores      = [evaluateBoardMax (depth-1) x (flipTile tile) (put board (flipTile tile) x) dim | x <- moves]
        moves       = closeMoves board

getRandomMove :: Dimensions -> IO Move
getRandomMove dim = do
    col <- randomRIO (5,dimM dim - 5) --Would fail for smaller boards
    row <- randomRIO (5,dimN dim - 5)
    return (row, col)
    
------------------------------------------------End-game score--------------------------------------------------
----------------------------------------------------------------------------------------------------------------
quickTileWins :: Board -> Dimensions -> Tile -> Move -> Bool
quickTileWins b dim t m =
   any (\col -> any (\row -> all (\k -> b??(row,col+k)   == t) [0..dimK']) [fst m]) [(snd m - dimK')..(snd m)] ||
   any (\col -> any (\row -> all (\k -> b??(row+k,col)   == t) [0..dimK']) [(fst m - dimK')..(fst m)]) [snd m] ||
   any (\col -> any (\row -> all (\k -> b??(row+k,col+k) == t) [0..dimK']) [(fst m - dimK')..(fst m)]) [(snd m - dimK')..(snd m)] ||
   any (\col -> any (\row -> all (\k -> b??(row-k,col+k) == t) [0..dimK']) [(fst m + dimK'), (fst m + dimK')-1..(fst m)]) [(snd m - dimK')..(snd m)]
   where
     dimK' = (dimK dim)-1

quickScoreBoard :: Tile -> Board -> Dimensions -> Move -> Maybe Int
quickScoreBoard tile board dim move
  | quickTileWins board dim tile move
  = Just 1000000000
  | otherwise
  = Nothing
  
------------------------------------------------In-game score---------------------------------------------------
----------------------------------------------------------------------------------------------------------------
{-
evaluateScore :: Board -> Tile -> Int
evaluateScore board tile = loopCount (1,1) tile 0 0 0

loopCount :: (Move -> Maybe Move) -> Tile -> Int -> Int -> Int
loopCount nextPos tile countT countE score

--Evaluates ends of the 4-tile combination and returns its score value depending on them
evaluateEnds :: Move -> Int


  where
    end1 = 

--Gives next position horizonally (returns nothing if out-of-bounds)
nextPositionH :: Move -> Dimensions -> Maybe Move
nextPositionH (x,y) dim nextPos
  | snd next > dimN dim
  = Nothing
  | otherwise
  = next
    where next = Just (x, y+1)

--Gives next position vertically (returns nothing if out-of-bounds)
nextPositionH :: Move -> Dimensions -> Maybe Move
nextPositionH (x,y) dim nextPos
  | fst next > dimM dim
  = Nothing
  | otherwise
  = next
    where next = Just (x+1, y)

--Gives next position diagonally (returns nothing if out-of-bounds)
nextPositionH :: Move -> Dimensions -> Maybe Move
nextPositionH (x,y) dim nextPos
  | (fst next > dimM dim)|| (snd next > dimN dim)
  = Nothing
  | otherwise
  = next
    where next = Just (x+1, y+1)
-}

data Pos = Pos {getRow::Int, getCol:: Int}
    deriving (Show, Eq, Ord)

getSymbol :: Pos -> Board -> Dimensions -> Maybe Tile
getSymbol position board dim
--fromMaybe Nothing (lookup position b)
  | (x < 1) || (x > dimN dim) || (y < 1) || (y > dimM dim) = Nothing
  | otherwise = Just $ board??(x,y)
  where 
    x = getRow position
    y = getCol position

--Evaluates end of the n-tile combination (which has an empty tile on the other end) and returns a score key depending on it
evaluateEnd1 :: Int -> Tile -> Maybe Tile -> Int
evaluateEnd1 n tile end
  | isNothing end || fromJust end == (flipTile tile)
  = scoreRating n
  | otherwise
  = scoreRating (n+10)

--Evaluates end of the n-tile combination (which has an non-empty tile on the other end) and returns a score key depending on it
evaluateEnd2 :: Int -> Tile -> Maybe Tile -> Int
evaluateEnd2 n tile end
  | isNothing end || fromJust end == (flipTile tile)
  = 0
  | otherwise
  = scoreRating n

countRow :: Pos -> Tile -> Board -> Dimensions -> Int -> Int
countRow position tile board dim number
  | getSymbol nextPosition board dim == Nothing
  = score
  | symbol == tile
  = countRow nextPosition tile board dim (number + 1)
  | otherwise
  = score + countRow nextPosition tile board dim 0
  where
    (Pos a b) = position
    nextPosition = Pos a (b+1)
    symbol = fromJust (getSymbol position board dim)
    score =
      if symbol == EmptyTile
        then evaluateEnd1 number tile $ getSymbol (Pos a (b-number-1)) board dim
        else if symbol == tile
          then evaluateEnd2 (number+1) tile $ getSymbol (Pos a (b-number)) board dim
          else evaluateEnd2 number tile $ getSymbol (Pos a (b-number-1)) board dim

countCol :: Pos -> Tile -> Board -> Dimensions -> Int -> Int
countCol position tile board dim number
  | getSymbol nextPosition board dim == Nothing
  = score
  | symbol == tile
  = countCol nextPosition tile board dim (number + 1)
  | otherwise
  = score + countCol nextPosition tile board dim 0
  where
    (Pos a b) = position
    nextPosition= Pos (a+1) b
    symbol = fromJust (getSymbol position board dim)
    score =
      if symbol == EmptyTile
        then evaluateEnd1 number tile $ getSymbol (Pos (a-number-1) b) board dim
        else if symbol == tile
          then evaluateEnd2 (number+1) tile $ getSymbol (Pos (a-number) b) board dim --change
          else evaluateEnd2 number tile $ getSymbol (Pos (a-number-1) b) board dim

countDiagN :: Pos -> Tile -> Board -> Dimensions -> Int -> Int
countDiagN position tile board dim number
  | getSymbol nextPosition board dim == Nothing
  = score
  | symbol == tile
  = countDiagN nextPosition tile board dim (number + 1)
  | otherwise
  = score + countDiagN nextPosition tile board dim 0
  where
    (Pos a b) = position
    nextPosition = Pos (a+1) (b+1)
    symbol = fromJust (getSymbol position board dim)
    score =
      if symbol == EmptyTile
        then evaluateEnd1 number tile $ getSymbol (Pos (a-number-1) (b-number-1)) board dim
        else if symbol == tile
          then evaluateEnd2 (number+1) tile $ getSymbol (Pos (a-number) (b-number)) board dim
          else evaluateEnd2 number tile $ getSymbol (Pos (a-number-1) (b-number-1)) board dim
          
countDiagP :: Pos -> Tile -> Board -> Dimensions -> Int -> Int
countDiagP position tile board dim number
  | getSymbol nextPosition board dim == Nothing
  = score
  | symbol == tile
  = countDiagP nextPosition tile board dim (number + 1)
  | otherwise
  = score + countDiagP nextPosition tile board dim 0
  where
    (Pos a b) = position
    nextPosition = Pos (a-1) (b+1)
    symbol = fromJust (getSymbol position board dim)
    score =
      if symbol == EmptyTile
        then (evaluateEnd1 number tile $ getSymbol (Pos (a+number+1) (b-number-1)) board dim)
        else if symbol == tile
          then (evaluateEnd2 (number+1) tile $ getSymbol (Pos (a+number) (b-number)) board dim)
          else (evaluateEnd2 number tile $ getSymbol (Pos (a+number+1) (b-number-1)) board dim)

scoreRating :: Int -> Int
scoreRating number
  | number == 2 = 10
  | number == 3 = 100
  | number == 4 = 500
  | number == 12 = 200
  | number == 13 = 1000
  | number == 14 = 10000
  | otherwise = 0

evaluateScore :: Board -> Dimensions -> Tile -> Int
evaluateScore board dim symbol
  | debug == True = trace ("valores: " ++ show ([scoreRow,scoreCol,scoreDiagN1 , scoreDiagN2 ,scoreDiagP1 , scoreDiagP2])) $ scoreRow + scoreCol + scoreDiagN1 + scoreDiagN2 + scoreDiagP1 + scoreDiagP2
  | otherwise = scoreRow + scoreCol + scoreDiagN1 + scoreDiagN2 + scoreDiagP1 + scoreDiagP2
  where 
    scoreRow =  sum $ map (\x -> countRow (Pos x 1)  symbol board dim 0) [1..dimM dim]
    scoreCol = sum $ map (\y -> countCol (Pos 1 y) symbol board dim 0) [1..dimN dim]
    
    scoreDiagN1 =  sum $ map (\y -> countDiagN (Pos 1 y) symbol board dim 0) [2..dimM dim]--0
    scoreDiagN2 =  sum $ map (\x -> countDiagN (Pos x 1) symbol board dim 0) [1..dimM dim]

    scoreDiagP1 =  sum $ map (\y -> countDiagP (Pos (dimN dim) y) symbol board dim 0) [2..dimN dim]--0
    scoreDiagP2 =  sum $ map (\x -> countDiagP (Pos x 1) symbol board dim 0)  [dimN dim, dimN dim -1..1]


