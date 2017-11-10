module Types where

import qualified Data.List 
import qualified Data.Maybe as M


-------------------------------------------------------------------------------
--- Configs--------------------------------------------------------------------
-------------------------------------------------------------------------------

data Config = Config
    { cDims :: Dimensions
    , cTimeout :: Int
    } deriving (Show)

data IntermediateConfig = IntermediateConfig
    { icBoardDimensions :: [Int]
    , icTilesToWin :: Int
    , icTimeout :: Int
    } deriving (Show)

toConfig :: (Maybe IntermediateConfig) -> (Maybe Config)
toConfig intermediate = case intermediate of
    Nothing -> Nothing
    Just im -> Just (Config (Dim ((icBoardDimensions im)!!0) ((icBoardDimensions im)!!1) (icTilesToWin im)) (icTimeout im))

-------------------------------------------------------------------------------
--- Board ---------------------------------------------------------------------
-------------------------------------------------------------------------------

data Tile = EmptyTile | X | O 
  deriving (Eq)

flipTile :: Tile -> Tile
flipTile X = O 
flipTile O = X 
flipTile _ = EmptyTile

type Move   = (Int,Int)

type Board  = [(Move, Tile)] 

data Dimensions = Dim {dimN :: Int, dimM :: Int, dimK :: Int} deriving (Show)


tempDim :: Dimensions
tempDim = Dim 10 10 5


(??) :: Board -> Move -> Tile
b??ij = M.fromMaybe EmptyTile (lookup ij b) 

emptyBoard :: Dimensions -> Board
emptyBoard dim = [((x,y), EmptyTile) | x <- [1..(dimN dim)], y <- [1..(dimM dim)]]

validMoves :: Board -> [Move]
validMoves board  = [ij | (ij, EmptyTile) <- board]

put :: Board -> Tile -> Move -> Board
put b t move = M.fromJust $ putMaybe b t move

putMaybe :: Board -> Tile -> Move -> Maybe Board
putMaybe b t xy = case b??xy of
               EmptyTile -> Just $ map (\(ij,tij) -> if ij == xy then (ij,t) else (ij,tij)) b 
               _         -> Nothing

-------------------------------------------------------------------------------
--- Player --------------------------------------------------------------------
-------------------------------------------------------------------------------

data Player = 
  Player { playerMove :: Tile -> Board -> Dimensions -> Int -> IO Move
         , playerName :: String
         } 

-------------------------------------------------------------------------------
--- Score ---------------------------------------------------------------------
-------------------------------------------------------------------------------

type Score = [(PlayerInfo, Int)]

showFinalScore :: Score -> String
showFinalScore [(p1,i1),(p2,i2)]
  = if i1 == i2 
      then "Its a tie!" 
      else ("The winner is " ++ show (if i1 < i2 then p2 else p1))

showScore [(p1,i1),(p2,i2)] 
  = show p1 ++ " : " ++ show i1 ++ " VS. " ++ show p2 ++ " : " ++ show i2 
showScore _ 
  = ""

incr :: PlayerInfo -> Score -> Score
incr pi xs = map (\(pj,sj) -> if pi == pj then (pj,sj+1) else (pj,sj)) xs 

-------------------------------------------------------------------------------
--- Result --------------------------------------------------------------------
-------------------------------------------------------------------------------

data Result = TimeOut PlayerInfo PlayerInfo | Wins Winner | Tie  | Invalid PlayerInfo PlayerInfo

-------------------------------------------------------------------------------
--- Player Info ---------------------------------------------------------------
-------------------------------------------------------------------------------

data PlayerInfo =  
  PI { playerInfoPlayer :: Player
     , playerInfoTile   :: Tile
     , playerInfoInt    :: Int
     }

type Winner = PlayerInfo

instance Eq PlayerInfo where
    p1 == p2 = playerInfoInt p1 == playerInfoInt p2 

instance Show Player where
  show = playerName

instance Show PlayerInfo where
  show pi 
    | pname /= "Computer" && pname /= "Human"
    =  pname 
    | otherwise 
    = "Player " ++ show (playerInfoInt pi) 
    where pname = playerName $ playerInfoPlayer pi


instance Show Tile where
  show EmptyTile = "     "
  show X         = "  X  "
  show O         = "  O  "

--instance Read Tile where
--  read ' ' = EmptyTile
--  read 'X' = X         
--  read 'O' = O        

showBoard :: Board -> Dimensions -> String
showBoard b dim = let blist = boardAsList b
                  in  unlines [Data.List.intercalate "|" row | row <- blist]
                  where
                    boardAsList b = [[show (b??(x,y)) | y <- [1..dimM dim]] | x <- [1..dimN dim]]

showTileNumbers :: Dimensions -> String
showTileNumbers dim = (unlines
                       [Data.List.intercalate "|" ["(" ++ show x ++ "," ++ show y ++ ")" |
                       y <- [1..dimM dim]] | x <- [1..dimN dim]])
