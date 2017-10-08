module Players (players) where
import Types
import Player.BestNext (playerBestNext)
import Player.BlindGuess (playerBlindGuess)
import Player.Computer (playerComputer)
import Player.Human (playerHuman)
import Player.Kevin (playerKevin)
import Player.Mugatu (playerMugatu)
import Player.TeamSinister (playerTeamSinister)
import Player.Theta (playerTheta)
players :: [(String, Player)]
players = [
  ("BestNext", playerBestNext),("BlindGuess", playerBlindGuess),("Computer", playerComputer),("Human", playerHuman),("Kevin", playerKevin),("Mugatu", playerMugatu),("TeamSinister", playerTeamSinister),("Theta", playerTheta)
  ]
