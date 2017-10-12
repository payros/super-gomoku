module Main where 

import System.IO
import System.Process
import Data.List 

import System.Random.Shuffle 
import System.IO.Unsafe

shuffleRandom :: [a] -> [a]
shuffleRandom xs = unsafePerformIO (shuffleM xs)

main :: IO ()
main = do 
  files <- readProcess "ls" ["Player"] []
  let players = (takeWhile (/='.')) <$> lines files  
  -- putStrLn ("Players:\n\n" ++ unlines players)
  putStrLn ("Shuffled Players :\n\n" ++ unlines (showP <$> zip [1..] (shuffleRandom players)))


showP (x,y) = show x ++ ": " ++ y