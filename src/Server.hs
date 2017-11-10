{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

import Yesod
import System.Environment
import Text.Lucius (luciusFile, luciusFileReload, luciusFileDebug)
import Text.Julius (juliusFile, juliusFileReload, juliusFileDebug, rawJS)
import Data.Text (unpack)
import Data.ByteString.Lazy.Char8 (pack)

import Data.Aeson

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
boardRows = [1..10]
boardCols = [1..10]
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


-- Given a board, and a bot name, gives the bot's next move.
postNextMoveR :: Handler String
postNextMoveR = do
    maybePlayer <- lookupPostParam "player"
    maybeBoard <- lookupPostParam "board"
    maybeTurn <- lookupPostParam "turn"
    maybeSettings <- lookupPostParam "settings"
    maybeStrategy <- case maybePlayer of
        Nothing -> error "Invalid Player Provided."
        Just p  -> return $ lookup (unpack p) players
    turn <- case maybeTurn of
        Nothing -> error "Invalid Turn Provided."
        Just t  -> return $ readTile $ unpack t
    maybeParsedSettings <- case maybeSettings of
        Nothing       -> error "No Settings Provided."
        Just settings -> return $ toConfig ((decode $ pack $ unpack settings) :: (Maybe IntermediateConfig))

    -- If we have received a board *and successfully parsed the settings, which contain the dimensions*, parse board.
    board <- case maybeBoard of
        Nothing  -> error "Invalid Board Provided."
        Just brd -> case maybeParsedSettings of
            Nothing       -> error "Could not parse settings"
            Just settings -> return $ readBoard (unpack brd) (cDims settings)

    -- If we found a strategy (player), parsed the settings correctly, and parsed the board correctly, get bot move.
    case maybeStrategy of
        Nothing       -> error "No bot with provided name."
        Just strategy -> case maybeParsedSettings of
            Nothing       -> error "Could not parse settings"
            Just settings -> liftIO $ moveToZeroIndexedStr (playerMove strategy turn board (cDims settings) (cTimeout settings))


-- This function just echos back the settings, but you can use it as a starting point if you want
postSyncSettingsR :: Handler String
postSyncSettingsR = do
    maybeSettings <- lookupPostParam "settings"
    return "TODO"


-- If a variable is optional, we can use .:? instead of .:
instance FromJSON IntermediateConfig where
    parseJSON (Object v) =
        IntermediateConfig <$> v .: "BOARD_DIMENSIONS"
                           <*> v .: "TILES_TO_WIN"
                           <*> v .: "TIMEOUT"

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

readBoard :: String -> Dimensions -> Board
readBoard xs dim = readBoardHelper xs dim 1 1

readBoardHelper :: String -> Dimensions -> Int -> Int -> Board
readBoardHelper [] _ _ _ = []
readBoardHelper (x:xs) dim row col
    | x == ','           = readBoardHelper xs dim row col
    | col == (dimM dim)  = ((row, col), readTile [x]):(readBoardHelper xs dim (row + 1)     1    )
    | otherwise          = ((row, col), readTile [x]):(readBoardHelper xs dim    row    (col + 1))


normalizePort :: [String] -> Int
normalizePort [] = 4000
normalizePort (port:xs)  = read port :: Int

main :: IO ()
main = do
    args <- getArgs
    warp (normalizePort args) GomokuServer