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

boardRows = [1..15]
boardCols = [1..15]

getHomeR :: Handler Html
getHomeR = defaultLayout $ do 
        $(whamletFile "./src/templates/home.hamlet")
        toWidget $(luciusFileReload "./src/templates/home.lucius")
        toWidget $(juliusFile "./src/templates/home.julius")

main :: IO ()
main = warp 3000 GomokuServer