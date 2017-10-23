{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import Yesod
import Text.Lucius (luciusFile, luciusFileReload, luciusFileDebug)
import Text.Julius (juliusFile, juliusFileReload, juliusFileDebug)

import Types 

data GomokuServer = GomokuServer

mkYesod "GomokuServer" [parseRoutes|
/ HomeR GET
|]

instance Yesod GomokuServer

boardRows = [1..dimM dim]
boardCols = [1..dimN dim]

getHomeR :: Handler Html
getHomeR = defaultLayout $ do
        setTitle "Super Gomoku"
        toWidgetHead [hamlet|<script src="https://ajax.googleapis.com/ajax/libs/jquery/1.6.2/jquery.min.js">|]
        $(whamletFile "./src/templates/home.hamlet")
        toWidget $(luciusFileReload "./src/templates/home.lucius")
        toWidget $(juliusFileReload "./src/templates/home.julius")

main :: IO ()
main = warp 3000 GomokuServer