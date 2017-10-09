module Player.SashankMichael (playerSashankMichael) where

import Types

teamMembers :: String
teamMembers = "Michael Anderjaska and Sashank Thupukari"

playerSashankMichael :: Player
playerSashankMichael = Player getNextMove "SashankMichael"

getNextMove :: Tile -> Board -> IO (Int, Int)
getNextMove t b = 
    case b of
        ((x, y), EmptyTile):xs -> return (x,y)
        _:xs -> getNextMove t xs