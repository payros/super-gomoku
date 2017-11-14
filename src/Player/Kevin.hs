module Player.Kevin (playerKevin) where
    
    import Types
    import Checks
    
    teamMembers :: String 
    teamMembers = "Kevin Chen"
    
    playerKevin :: Player 
    playerKevin = Player firstMove "Kevin"

    firstMove :: Tile -> Board -> Dimensions -> Int -> IO Move
    firstMove tile board dim time =
        let firstPossibleMove = head $ validMoves board
        in return firstPossibleMove