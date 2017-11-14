module Player.Mugatu (playerMugatu) where
    
    import Types (Player(..), Tile, Board, Move, Dimensions, validMoves)
    import Checks 

    teamMembers :: String 
    teamMembers = "Phillip Wise"
    
    playerMugatu :: Player 
    playerMugatu = Player crazyPills "Mugatu"

    crazyPills :: Tile -> Board -> Dimensions -> Int -> IO Move
    crazyPills tile board dim time = return $ head $ validMoves board