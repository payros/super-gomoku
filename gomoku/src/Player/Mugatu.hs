module Player.Mugatu (playerMugatu) where
    
    import Types (Player(..), Tile, Board, Move, validMoves)
    import Checks 

    teamMembers :: String 
    teamMembers = "Phillip Wise"
    
    playerMugatu :: Player 
    playerMugatu = Player crazyPills "Mugatu"

    crazyPills :: Tile -> Board -> IO Move
    crazyPills tile board = return $ head $ validMoves board