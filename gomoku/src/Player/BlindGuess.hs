-- | Player that "thinks really hard about where to move next"

module Player.BlindGuess (playerBlindGuess) where 

import Types  (Player(..), Tile, Board, Move, validMoves, put, flipTile)
import Checks (scoreBoard)

teamMembers :: String
teamMembers = "Braeden Mollot"

playerBlindGuess :: Player 
playerBlindGuess = Player strategy "Blind Guess"


strategy :: Tile -> Board -> IO Move
strategy tile board = return $ snd $ maximum scoredMoves 
  where
    scoredMoves = zip scores moves
    scores      = map (evalMove tile board) moves 
    moves       = validMoves board


evalMove :: Tile -> Board -> Move -> Int
evalMove tile board move = max myScore theirScore
  where
    theirScore = case scoreBoard (flipTile tile) revBoard of
      Just x  -> x
      Nothing -> 0
    myScore = case scoreBoard tile newBoard of
      Just x  -> x
      Nothing -> 0
    revBoard = put board (flipTile tile) move
    newBoard = put board tile move
