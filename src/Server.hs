{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DeriveGeneric #-}

import Yesod
import Text.Lucius (luciusFile, luciusFileReload, luciusFileDebug)
import Text.Julius (juliusFile, juliusFileReload, juliusFileDebug, rawJS)
import Data.Text (Text, unpack)
import Data.Maybe
import Data.Aeson
import Data.Typeable
import Debug.Trace
import GHC.Generics

import Players
import Types 

data GomokuServer = GomokuServer

data Person = Person
    { name :: Text
    , age  :: Int
    }
    deriving (Show, Generic)

mkYesod "GomokuServer" [parseRoutes|
/         HomeR     GET
--/nextMove NextMoveR POST
|]

instance FromJSON Person
instance ToJSON Person
instance Yesod GomokuServer

boardRows = [1..dimM dim]
boardCols = [1..dimN dim]
playersList = [fst x | x <- players] 

mimeType :: ContentType
mimeType = "text/haskell-show"

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
        setTitle "Super Gomoku"
        toWidgetHead [hamlet| <link rel="stylesheet" href="https://ajax.googleapis.com/ajax/libs/jqueryui/1.12.1/themes/smoothness/jquery-ui.css"> |]
        toWidgetHead [hamlet| <script src="https://ajax.googleapis.com/ajax/libs/jquery/1.7/jquery.min.js"> |]
        toWidgetHead [hamlet| <script src="https://ajax.googleapis.com/ajax/libs/jqueryui/1.12.1/jquery-ui.min.js"> |]
        $(whamletFile "./src/templates/home.hamlet")
        toWidget $(luciusFileReload "./src/templates/home.lucius")
        toWidget $(juliusFileReload "./src/templates/home.julius")

--postNextMoveR :: Handler Value
--postNextMoveR = returnJson person
--	where
--	    person = Person "Michael" 28

main :: IO ()
main = warp 4000 GomokuServer