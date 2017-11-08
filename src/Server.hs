{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

import Yesod
import System.Environment
import Text.Lucius (luciusFile, luciusFileReload, luciusFileDebug)
import Text.Julius (juliusFile, juliusFileReload, juliusFileDebug, rawJS)
import Data.Text (unpack)

import Players
import Types

data GomokuServer = GomokuServer

mkYesod "GomokuServer" [parseRoutes|
/             HomeR         GET
/nextMove     NextMoveR     POST
/syncSettings SyncSettingsR POST
|]

instance Yesod GomokuServer

-- Variables accessed by front-end
boardRows = [1..dimM dim]
boardCols = [1..dimN dim]
playersList = [fst x | x <- players]

mimeType :: ContentType
mimeType = "text/haskell-show"

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
        setTitle "Super Gomoku"
        toWidgetHead [hamlet| <link rel="stylesheet" href="https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css"> |]
        toWidgetHead [hamlet| <link rel="stylesheet" href="https://ajax.googleapis.com/ajax/libs/jqueryui/1.12.1/themes/smoothness/jquery-ui.css"> |]
        toWidgetHead [hamlet| <script src="https://ajax.googleapis.com/ajax/libs/jquery/1.7/jquery.min.js"> |]
        toWidgetHead [hamlet| <script src="https://ajax.googleapis.com/ajax/libs/jqueryui/1.12.1/jquery-ui.min.js"> |]
        toWidgetHead [hamlet| <script src="https://cdnjs.cloudflare.com/ajax/libs/bootstrap-3-typeahead/4.0.2/bootstrap3-typeahead.min.js"> |]
        $(whamletFile "./src/templates/home.hamlet")
        toWidget $(luciusFileReload "./src/templates/home.lucius")
        toWidget $(juliusFileReload "./src/templates/home.julius")

postNextMoveR :: Handler String
postNextMoveR = do
    maybePlayer <- lookupPostParam "player"
    maybeBoard <- lookupPostParam "board"
    maybeTurn <- lookupPostParam "turn"
    maybeStrategy <- case maybePlayer of
        Nothing -> error "Invalid Player Provided."
        Just p -> return $ lookup (unpack p) players
    board <- case maybeBoard of
        Nothing -> error "Invalid Board Provided."
        Just b -> return $ readBoard $ unpack b
    turn <- case maybeTurn of
        Nothing -> error "Invalid Turn Provided."
        Just t -> return $ readTile $ unpack t
    case maybeStrategy of
        Nothing -> error "No bot with provided name."
        Just strategy -> liftIO $ moveToZeroIndexedStr $ playerMove strategy turn board

-- This function just echos back the settings, but you can use it as a starting point if you want
postSyncSettingsR :: Handler String
postSyncSettingsR = do
    maybeSettings <- lookupPostParam "settings"
    case maybeSettings of
        Nothing -> return "Nothing to Update"
        Just settings -> return $ unpack settings

-- Subtracts one from row and col of move so it is 0-indexed, and converts to an IO String
moveToZeroIndexedStr :: (IO Move) -> (IO String)
moveToZeroIndexedStr move = do
    m <- move
    return $ show ((fst m) - 1, (snd m) - 1)

readTile :: String -> Tile
readTile s
    | s == "X"  = X
    | s == "O"  = O
    | otherwise = EmptyTile

readBoard :: String -> Board
readBoard xs = readBoardHelper xs 1 1

readBoardHelper :: String -> Int -> Int -> Board
readBoardHelper [] _ _ = []
readBoardHelper (x:xs) row col
    | x == ','          = readBoardHelper xs row col
    | col == (dimM dim) = ((row, col), readTile [x]):(readBoardHelper xs (row + 1)     1    )
    | otherwise         = ((row, col), readTile [x]):(readBoardHelper xs    row    (col + 1))

normalizePort :: [String] -> Int
normalizePort [] = 4000
normalizePort (port:xs)  = read port :: Int

main :: IO ()
main = do
    args <- getArgs
    warp (normalizePort args) GomokuServer