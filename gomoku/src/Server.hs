{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
import Yesod
import Text.Lucius (luciusFile, luciusFileDebug)
import Text.Julius (juliusFile, juliusFileDebug)

data GomokuServer = GomokuServer

mkYesod "GomokuServer" [parseRoutes|
/ HomeR GET
|]

instance Yesod GomokuServer

getHomeR :: Handler Html
getHomeR = defaultLayout $ do 
        $(whamletFile "./src/templates/home.hamlet")
        toWidget $(luciusFile "./src/templates/home.lucius")
        toWidget $(juliusFile "./src/templates/home.julius")

main :: IO ()
main = warp 3000 GomokuServer