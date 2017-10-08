module Player.Kevin (playerKevin) where
    
    import Types
    import Checks
    
    teamMembers :: String 
    teamMembers = "Kevin Chen"
    
    playerKevin :: Player 
    playerKevin = Player firstMove "Kevin"

    firstMove :: Tile -> Board -> IO Move
    firstMove tile board = 
        let firstPossibleMove = head $ validMoves board
        in return firstPossibleMove