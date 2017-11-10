module Checks where

import Prelude hiding ((!!))
import qualified Data.List 

import Types 

p1wins, p2wins :: Board -> Dimensions -> Bool
p1wins b dim = tileWins b dim X
p2wins b dim = tileWins b dim O

tileWins :: Board -> Dimensions -> Tile -> Bool
tileWins b dim t =
   any (\col -> any (\row -> all (\k -> b??(row,col+k)   == t) [0..dimK dim-1]) [1..dimM dim]) [1..dimN dim] ||
   any (\col -> any (\row -> all (\k -> b??(row+k,col)   == t) [0..dimK dim-1]) [1..dimM dim]) [1..dimN dim] ||
   any (\col -> any (\row -> all (\k -> b??(row+k,col+k) == t) [0..dimK dim-1]) [1..dimM dim]) [1..dimN dim] ||
   any (\col -> any (\row -> all (\k -> b??(row-k,col+k) == t) [0..dimK dim-1]) [1..dimM dim]) [1..dimN dim] 


winningMoves :: Board -> Dimensions -> Tile -> [Move]
winningMoves b dim t =
   concatMap (\col -> concatMap (\row -> if all (\k -> b??(row,col+k)   == t) [0..dimK dim-1] then [(row,col+k)   | k <- [0..dimK dim-1]] else []) [1..dimM dim]) [1..dimN dim] ++
   concatMap (\col -> concatMap (\row -> if all (\k -> b??(row+k,col)   == t) [0..dimK dim-1] then [(row+k,col)   | k <- [0..dimK dim-1]] else []) [1..dimM dim]) [1..dimN dim] ++
   concatMap (\col -> concatMap (\row -> if all (\k -> b??(row+k,col+k) == t) [0..dimK dim-1] then [(row+k,col+k) | k <- [0..dimK dim-1]] else []) [1..dimM dim]) [1..dimN dim] ++
   concatMap (\col -> concatMap (\row -> if all (\k -> b??(row-k,col+k) == t) [0..dimK dim-1] then [(row-k,col+k) | k <- [0..dimK dim-1]] else []) [1..dimM dim]) [1..dimN dim] 


showWinningBoard :: Board -> Dimensions -> Tile -> String
showWinningBoard b dim t = unlines [Data.List.intercalate "|" row | row <- blist]
  where
     boardAsList b = [[if (x,y) `elem` wins then color (show (b??(x,y))) else show (b??(x,y)) | y <- [1..dimM dim]] | x <- [1..dimN dim]]
     blist         = boardAsList b
     wins          = winningMoves b dim t
     color x       = "\x1b[32m" ++ x ++ "\x1b[0m"


showBoardNew :: Move -> Board -> Dimensions -> String
showBoardNew m b dim = unlines [Data.List.intercalate "|" row | row <- blist]
  where
     boardAsList b = [[if (x,y) == m then color (show (b??(x,y))) else show (b??(x,y)) | y <- [1..dimM dim]] | x <- [1..dimN dim]]
     blist         = boardAsList b
     color x       = "\x1b[31m" ++ x ++ "\x1b[0m"


checkFull :: Board -> Dimensions -> Bool
checkFull b dim = all (\row -> all (\col -> b??(row, col) /= EmptyTile) [1..dimM dim]) [1..dimN dim]


scoreBoard :: Tile -> Board -> Dimensions -> Maybe Int
scoreBoard tile board dim
  | tileWins board dim tile
  = Just 1
  | tileWins board dim (flipTile tile)
  = Just (-1) 
  | checkFull board dim
  = Just 0
  | otherwise
  = Nothing 
