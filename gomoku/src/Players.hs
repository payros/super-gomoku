module Players (players) where
import Types
import Player.BestNext (playerBestNext)
import Player.BlindGuess (playerBlindGuess)
import Player.Computer (playerComputer)
import Player.Human (playerHuman)
import Player.Kevin (playerKevin)
import Player.LazyMinMax (playerLazyMinMax)
import Player.Mugatu (playerMugatu)
import Player.SashankMichael (playerSashankMichael)
import Player.Sigma (playerSigma)
import Player.TeamSinister (playerTeamSinister)
players :: [(String, Player)]
players = [
  ("BestNext", playerBestNext),("BlindGuess", playerBlindGuess),("Computer", playerComputer),("Human", playerHuman),("Kevin", playerKevin),("LazyMinMax", playerLazyMinMax),("Mugatu", playerMugatu),("SashankMichael", playerSashankMichael),("Sigma", playerSigma),("TeamSinister", playerTeamSinister)
  ]
