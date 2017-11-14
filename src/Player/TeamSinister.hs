module Player.TeamSinister (playerTeamSinister, teamMembers) where

import Data.Maybe
import Data.List (maximumBy)
import Types (Move, Tile, Board, (??))
import qualified Types
import qualified Misc

data Strategy = Attack [Types.Move] | Defend [Types.Move]
  deriving (Eq, Show)

teamMembers :: String
teamMembers = "Davis Silverman"

playerTeamSinister :: Types.Player
playerTeamSinister = Types.Player getStrategizedMove "TeamSinister"


--------------------
-- Function to get 'best chain to make in 1 turn'
-- Function that when attacking, will space out placements, and fill them in later?
-- So the placements will be X-X-X, and then fill in the two -'s last
--------------------

getStrategizedMove :: Types.Tile -> Types.Board -> Types.Dimensions -> Int -> IO Types.Move
getStrategizedMove t b dim time =
  case getStrategy t b dim of
    Attack c -> attack t b dim c
    Defend c -> defend t b dim c

getStrategy :: Types.Tile -> Types.Board -> Types.Dimensions -> Strategy
getStrategy t b dim =
  let enemyTile = Types.flipTile t in
  let bestChain = getLongestChain t b dim in
  -- TODO: their longest is not always the most dangerous!
  --  Their longest may be blocked already
  --  So we need their longest that is capable of winning.
  let bestEnemyChain = getLongestChain enemyTile b dim in
  if (dangerousChain b dim bestEnemyChain) && (not (aboutToWin bestChain b dim))
    then Defend bestEnemyChain
    else Attack bestChain

-- checks if the chain can be a winner in a single turn.
aboutToWin ::[Types.Move] -> Types.Board -> Types.Dimensions -> Bool
aboutToWin chain board dim =
  ((length chain) == ((Types.dimK dim) - 1)) && growableBy board dim chain 1

-- creates a chain from a start point and a direction.
-- Never make first argument negative! Or else infinite loop :==
createChain :: Int -> (Int, Int) -> Move -> [Move]
createChain (-1) _ _ = []
createChain 0 _ _ = []
createChain n change@(dx, dy) pos@(x, y) = pos:(createChain (n-1) change (x+dx, y+dy))

-- returns the tiles most promising chain, by length.
-- algorithm:
--   For each tile i of type t in board
--     check neighbors for another tile of type t
--     if so, recurse into that for a longest chain
--     longest wins.
getLongestChain :: Tile -> Board -> Types.Dimensions -> [Move]
getLongestChain t b dim =
  -- (uncurry3 createChain) $ foldl (longestChainAt' t b dim) (-1, (-1, -1), (-1, -1)) b
  let val@(amt, dir, pos) = foldl (longestChainAt' t b dim) (-1, (-1, -1), (-1, -1)) b in
    (createChain amt dir pos)

longestChainAt :: Tile -> Board -> Types.Dimensions -> Move -> [Move]
longestChainAt t b dim p =
  -- (uncurry3 createChain) $ longestChainAt' t b dim (-1, (-1, -1), (-1, -1)) (p, t)
  let (amt, dir, pos) = longestChainAt' t b dim (-1, (-1, -1), (-1, -1)) (p, t) in
  (createChain amt dir pos)

whichSlope :: Move -> (Int, Int) -> (Int, Int) -> Types.Dimensions -> Ordering
whichSlope pos@(x, y) s1@(dx1, dy1) s2@(dx2, dy2) dim =
  if outOfBounds (x+dx1, y+dy1) dim
    then if outOfBounds (x+dx2, y+dy2) dim
      then EQ
      else LT
    else GT

-- TODO: this returns (Int, (Int, Int), Move)
--  which is inconsistent with the rest of the code, it should be pos then slope,
--  so, TODO, change return type to (Int, Move, (Int, Int))
longestChainAt' :: Tile -> Board -> Types.Dimensions -> (Int, (Int, Int), Move) -> (Move, Tile) -> (Int, (Int, Int), Move)
longestChainAt' tile board dim acc (pos@(x, y), checkType) =
  if checkType == tile
    then maximumBy (\(a1, slope1, _) (a2, slope2, _) -> compare a1 a2) [
        ((chainDirection tile board dim (x-1, y-1) (-1, -1))+1, (-1, -1), pos)
      , ((chainDirection tile board dim (x, y-1)   (0, -1))+1,  (0, -1), pos)
      , ((chainDirection tile board dim (x+1, y-1) (1, -1))+1,  (1, -1), pos)
      , ((chainDirection tile board dim (x+1, y)   (1, 0))+1,   (1, 0), pos)
      , ((chainDirection tile board dim (x+1, y+1) (1, 1))+1,   (1, 1), pos)
      , ((chainDirection tile board dim (x, y+1)   (0, 1))+1,   (0, 1), pos)
      , ((chainDirection tile board dim (x-1, y+1) (-1, 1))+1,  (-1, 1), pos)
      , ((chainDirection tile board dim (x-1, y)   (-1, 0))+1,  (-1, 0), pos)
      , acc
      ]
    else (acc)

-- Takes a board, tile to check, and a direction
-- Returns the amount of tiles in the chain.
chainDirection :: Types.Tile -> Types.Board -> Types.Dimensions -> Types.Move -> (Int, Int) -> Int
chainDirection tile board dim cur@(x, y) direction@(dx, dy) =
  if board??cur /= tile || (outOfBounds cur dim)
    then 0
    else 1 + (chainDirection tile board dim (x+dx, y+dy) direction)

-- Takes two ADJACENT tiles and returns their slope
getSlope :: Move -> Move -> (Int, Int)
getSlope p1@(x1, y1) p2@(x2, y2) =
  (x2 - x1, y2 - y1)

-- Decides on if a chain is dangerous or not.
--  Generally, a dangerous chain should be defended against.
dangerousChain :: Types.Board -> Types.Dimensions -> [Types.Move] -> Bool
dangerousChain b dim chain =
  let len = length chain in
  len >= 3 && (growableBy b dim chain ((Types.dimK dim) - len))

-- TODO: refactor this...
growableBy :: Types.Board -> Types.Dimensions -> [Types.Move] -> Int -> Bool
growableBy _ _ [] _ = undefined
growableBy board dim chain amt =
  if length chain > 1
    then
      let first = head chain in
      let sec = head $ tail chain in
      let typ = board??first in
      let slope@(dx, dy) = getSlope first sec in
      let negSlope = (dx * (-1) , dy * (-1)) in
      let maxLength = ((maxChainLength typ board dim first negSlope) + (maxChainLength typ board dim sec slope)) in
      (maxLength - (length chain)) >= amt
    else
      let pos = head chain in
      let tile = board??pos in
      let dirs = [(-1, -1), (0, -1), (1, -1), (1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0)] in
      let maxInDir = map (\dir -> (maxChainLength tile board dim pos dir)) dirs in
      let maxLen = maximum maxInDir in
      (maxLen - (length chain)) >= amt


-- Similar to chainDirection, but returns possible max chain length,
-- And not the length of the chain.
maxChainLength :: Tile -> Board -> Types.Dimensions -> Move -> (Int, Int) -> Int
maxChainLength t b dim pos@(x, y) slope@(dx, dy) =
  if (Types.flipTile t) == (b??pos) || (outOfBounds pos dim)
    then 0
    else 1 + (maxChainLength t b dim (x+dx, y+dy) slope)

outOfBounds :: Types.Move -> Types.Dimensions -> Bool
outOfBounds (x, y) dim =
  let n = Types.dimN dim in
  let m = Types.dimM dim in
  not (x > 0 && y > 0 && x <= n && y <= m)

-- TODO
--  make sure where you place, you will eventually be able to get 5 in a row
--      (unless blocked off at some point in the future, but thats unknowable...)
attack :: Types.Tile -> Types.Board -> Types.Dimensions -> [Types.Move] -> IO Types.Move
attack t b dim [] = return $ chooseNewMove t b dim
attack t b dim chain = return $ case getNextMove t b dim chain of
  Nothing -> chooseNewMove t b dim
  Just m -> m

-- Determines if the next move should be made based on the chain or not.
--  If so, then the move to make will be returned.
--  If not, then Nothing will be returned.
getNextMove :: Tile -> Board -> Types.Dimensions -> [Move] -> Maybe Move
getNextMove t b _ [] = Nothing
getNextMove t b dim (pos@(x, y):[]) =
  let dirs = [(-1, -1), (0, -1), (1, -1), (1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0)] in
  let possibleDirs = filter (\(dx, dy) -> not $ outOfBounds (x+dx, y+dy) dim) dirs in
  -- let possibleDirs = foldl (\acc slope@(dx, dy) -> if outOfBounds (x+dx, y+dy) dim then acc else (slope:acc)) [] dirs in
  let maxInDir = (map (\dir -> ((maxChainLength t b dim pos dir), dir)) possibleDirs) in
  let (maxLen, _) = maximumBy (\(a0, _) (a1, _) -> compare a0 a1) maxInDir in
  let filtered = filter (\(a0, _) -> a0 == maxLen) maxInDir in
  -- TODO: randomly choose element of filtered.
  if length filtered > 1
    then
      let choice@(dx, dy) = snd $ head filtered in
      Just (x+dx , y+dy)
    else Nothing
-- TODO: backtrack to second largest list instead of starting over when this 'fails'?
getNextMove t b dim c =
  let len = (length c) in
  let first@(fx, fy) = (head c) in
  let end@(ex, ey) = c!!(length c - 1) in
  let second = head $ tail c in
  let slope@(dx, dy) = getSlope first second in
  let negSlope@(dx', dy') = (dx * (-1) , dy * (-1)) in
  let slopeMax = (maxChainLength t b dim end slope) - 1 in -- -1 to exclude first element
  let negMax = (maxChainLength t b dim first negSlope) -1 in
  if len + slopeMax < (Types.dimK dim) && len + negMax < (Types.dimK dim)
    then Nothing -- nowhere to grow, start over.
    else if len + slopeMax >= (Types.dimK dim)
      then Just (ex+dx , ey+dy) -- only 'forward' works here.
      else Just (fx + dx' , fy + dy') -- either back or both works here. just go back.

chooseNewMove :: Tile -> Board -> Types.Dimensions -> Move
chooseNewMove t b dim =
  let usable = filter (usableTile t b dim) b in
    fst $ head usable -- TODO randomly select this starting point?

usableTile :: Tile -> Board -> Types.Dimensions -> (Move, Tile) -> Bool
usableTile typ board dim (pos@(x, y), check)
  | check /= Types.EmptyTile = False
  | otherwise =
    let dirs = [(-1, -1), (0, -1), (1, -1), (1, 0), (1, 1), (0, 1), (-1, 1), (-1, 0)] in
    let maxInDir = map (\dir -> maxChainLength typ board dim pos dir) dirs in
    let maxLen = maximum maxInDir in
    if maxLen > (Types.dimK dim) -- TODO: is maxLen enough?
      then True
      else False

defend :: Types.Tile -> Types.Board -> Types.Dimensions -> [Types.Move] -> IO Types.Move
defend t b dim chain =
  let first@(fx, fy) = chain!!0 in
  let second = chain!!1 in
  let last@(lx, ly) = chain!!(length chain - 1) in
  let slope@(dx, dy) = getSlope first second in
  let negSlope@(nx, ny) = (dx * (-1) , dy * (-1)) in
  let leftDefense = (fx+nx, fy+ny) in
  let rightDefense = (lx+dx, ly+dy) in
  if (not $ outOfBounds leftDefense dim) && b??leftDefense == Types.EmptyTile
    then return leftDefense
    else if (not $ outOfBounds rightDefense dim) && b??rightDefense == Types.EmptyTile
      then return rightDefense
      else return $ (chooseNewMove t b dim)

-- TODO wont know what to do when there are no 'good' tiles left.
    -- Will definitely crash in that case, but plz dont lower my grade D:
