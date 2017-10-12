module Player.Theta (playerTheta) where

import System.Random (randomRIO)

import Types

teamMembers :: String
teamMembers = "Suteerth"

playerTheta :: Player
playerTheta =  Player rng_plays "Theta"

rng_plays :: Tile -> Board -> IO Move
rng_plays t b = do
    g  <- (guess (length (validMoves b)))
    return $ (validMoves b) !! g

guess:: Int -> IO Int
guess sup = randomRIO (0, sup-1)
