-- Chooses random element from validMoves

module Player.Sigma (playerSigma) where

import System.Random (randomRIO)

import Types

teamMembers :: String
teamMembers = "Colin Burr"

playerSigma :: Player
playerSigma = Player strategy "Sigma"


strategy :: Tile -> Board -> IO (Int, Int)
strategy t b = do
  x <- randomRIO (0, length (validMoves b) - 1)
  return $ (validMoves b) !! x
