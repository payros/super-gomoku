module Players (players) where
import Types
import Player.BestNext (playerBestNext)
import Player.BlindGuess (playerBlindGuess)
import Player.HeadlessChicken (playerHeadlessChicken)
import Player.Hybrid (playerHybrid)
import Player.Kevin (playerKevin)
import Player.KunkelOwen (playerKunkelOwen)
import Player.LazyMinMax (playerLazyMinMax)
import Player.Mikey (playerMikey)
import Player.Mugatu (playerMugatu)
import Player.Notemotives (playerNotemotives)
import Player.Rooster (playerRooster)
import Player.SashankMichael (playerSashankMichael)
import Player.Sigma (playerSigma)
import Player.TeamSinister (playerTeamSinister)
import Player.Theta (playerTheta)
players :: [(String, Player)]
players = [
  ("BestNext", playerBestNext),("BlindGuess", playerBlindGuess),("HeadlessChicken", playerHeadlessChicken),("Hybrid", playerHybrid),("Kevin", playerKevin),("KunkelOwen", playerKunkelOwen),("LazyMinMax", playerLazyMinMax),("Mikey", playerMikey),("Mugatu", playerMugatu),("Notemotives", playerNotemotives),("Rooster", playerRooster),("SashankMichael", playerSashankMichael),("Sigma", playerSigma),("TeamSinister", playerTeamSinister),("Theta", playerTheta)
  ]
