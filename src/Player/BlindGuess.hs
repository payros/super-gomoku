-- | Player that "thinks really hard about where to move next"

module Player.BlindGuess (playerBlindGuess) where 

import Types  (Player(..), Tile, Board, Move, Dimensions, validMoves, put, flipTile)
import Checks (scoreBoard)

teamMembers :: String
teamMembers = "Braeden Mollot"

playerBlindGuess :: Player 
playerBlindGuess = Player strategy "Blind Guess"


strategy :: Tile -> Board -> Dimensions -> Int -> IO Move
strategy tile board dim time = return $ snd $ maximum scoredMoves
  where
    scoredMoves = zip scores moves
    scores      = map (evalMove tile board dim) moves
    moves       = validMoves board


evalMove :: Tile -> Board -> Dimensions -> Move -> Int
evalMove tile board dim move = max myScore theirScore
  where
    theirScore = case scoreBoard (flipTile tile) revBoard dim of
      Just x  -> x
      Nothing -> 0
    myScore = case scoreBoard tile newBoard dim of
      Just x  -> x
      Nothing -> 0
    revBoard = put board (flipTile tile) move
    newBoard = put board tile move
