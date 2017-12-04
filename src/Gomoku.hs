module Main where

import Types
import Checks
import Misc
import Players

import Control.Concurrent
import System.Exit
import System.Environment
import Control.Timeout
import Data.Time.Units (Second)
import Data.List  (lookup)


main :: IO ()
main = do
    (player1,player2) <-  getArgs >>= getPlayers
    putStrLn "This is the Gomoku game."
    -- rounds  <- prompt "How many rounds should we play?"
    score   <- playRounds 5 player1  player2 (Dim 10 10 5) 30
    putStrLn $ showFinalScore score


getPlayers :: [String] -> IO (Player, Player)
getPlayers input =
  case input of
    (n1:n2:_) -> (,) <$> lookupPlayer n1 <*> lookupPlayer n2
    _         -> putStrLn "Give at least two player names" >> exitFailure

lookupPlayer :: String -> IO Player
lookupPlayer name =
  case lookup name players of
    Just p -> return p
    _      -> err
  where
    err = putStrLn ("Player " ++ show name ++ "does not exists.") >> exitFailure

playRounds :: Int -> Player -> Player -> Dimensions -> Int -> IO Score
playRounds rounds player1 player2 dim time =
  foldM (playRound pi1 pi2 dim time) [(pi1,0),(pi2,0)] [1..rounds]
  where
    pi1 = PI player1 X 1
    pi2 = PI player2 O 2

playRound :: PlayerInfo -> PlayerInfo -> Dimensions -> Int -> Score -> Int -> IO Score
playRound p1 p2 dim time score i = do
   putStrLn ("Score:: " ++ showScore score)
   putStrLn ("Round " ++ show i ++ "!")
   putStrLn ((if (i `mod` 2 == 0) then show p2 else show p1)  ++ " plays first")
   if i /= 1 then putStrLn ("Ready for round " ++ show i ++ "? [Y/N]") >> getLine >> return () else return ()
   result <- if (i `mod` 2 == 0) then play p2 p1 (emptyBoard dim) dim time else play p1 p2 (emptyBoard dim) dim time
   case result of
      TimeOut p p' -> putStrLn (show p ++ " timed out after 30sec!\n\n") >> return (incr p' score)
      Invalid p p' -> putStrLn (show p ++ " made an invalid move!\n\n")  >> return (incr p' score)
      Wins p       -> putStrLn (show p ++ " wins!\n\n") >> return (incr p score)
      Tie          -> putStrLn "Its a tie!\n\n" >> return score


play :: PlayerInfo -> PlayerInfo -> Board -> Dimensions -> Int -> IO Result
play pi1@(PI p1 t1 _) pi2 board dim time = do
  timedMove <- timeout ((fromInteger (toInteger time))::Second) $ (playerMove p1) t1 board dim time
  case timedMove of
    Nothing   -> return $ TimeOut pi1 pi2
    Just move ->
      case putMaybe board t1 move of
        Nothing -> putStrLn ("Invalid move. " ++ show move) >> return (Invalid pi1 pi2)
        Just b  -> if tileWins b dim t1
                      then putStrLn (showWinningBoard b dim t1) >> return (Wins pi1)
                      else do putStrLn $ showBoardNew move b dim
                              -- threadDelay 100000
                              if checkFull b dim
                                 then return Tie
                                 else play pi2 pi1 b dim time


