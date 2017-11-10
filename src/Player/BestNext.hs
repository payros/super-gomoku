-- | Player that pays the best next move

module Player.BestNext (playerBestNext) where 

import Types  (Player(..), Tile, Board, Dimensions, Move, validMoves, put)
import Checks (scoreBoard)

playerBestNext :: Player 
playerBestNext = Player strategy "Best Next"


strategy :: Tile -> Board -> Dimensions -> Int -> IO Move
strategy tile board dim time = return $ snd $ maximum scoredMoves
  where
    scoredMoves = zip scores moves
    scores      = map (\move -> (evaluateBoard tile (put board tile move) dim)) moves
    moves       = validMoves board


evaluateBoard :: Tile -> Board -> Dimensions -> Int
evaluateBoard tile board dim
  | Just i <- scoreBoard tile board dim
  =  i
  | otherwise
  = -2
