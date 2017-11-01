{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE DeriveGeneric         #-}
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

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
        setTitle "Super Gomoku"
        toWidgetHead [hamlet| <link rel="stylesheet" href="https://ajax.googleapis.com/ajax/libs/jqueryui/1.12.1/themes/smoothness/jquery-ui.css"> |]
        toWidgetHead [hamlet| <script src="https://ajax.googleapis.com/ajax/libs/jquery/1.7/jquery.min.js"> |]
        toWidgetHead [hamlet| <script src="https://ajax.googleapis.com/ajax/libs/jqueryui/1.12.1/jquery-ui.min.js"> |]
        $(whamletFile "./src/templates/home.hamlet")
        toWidget $(luciusFileReload "./src/templates/home.lucius")
        toWidget $(juliusFileReload "./src/templates/home.julius")


mimeType :: ContentType
mimeType = "text/haskell-show"

--postNextMoveR :: Handler Value
--postNextMoveR = returnJson person
--	where
--	    person = Person "Michael" 28

-- postNextMoveR :: Handler TypedContent
-- postNextMoveR = do
--   params <- getPostParams
--   moveReq <- fromJSON params
--   parsedBoard <- newFUnction board
-- --   post <- requireJsonBody :: Handler Post
--   -- do the read thing
--   return $ TypedContent mimeType $ toContent $ show move
--   where
--     move = (4,5)

-- define move's show '45'

data MoveRequest =
  MoveRequest { player :: String
              , board  :: [String]
              , tile   :: String
              } deriving (Show, Generic)

instance FromJSON MoveRequest

-- write instance of read for tile

-- instance FromJSON MoveRequest where
--  parseJSON (Object v) =
--     MoveRequest <$> v .:? "player"
--                 <*> v .:  "lastName"
--                 <*> v .:  "age"
--                 <*> v .:  "likesPizza"
-- instance FromJSON MoveRequest
-- instance ToJSON MoveRequest

data MoveReply =
  MoveReply { move :: Move
            } deriving (Show,Generic)

instance ToJSON MoveReply

-- getJSON :: IO B.ByteString
-- getJSON = B.readFile jsonFile


main :: IO ()
main = warp 3000 GomokuServer
