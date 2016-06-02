module Game where

-- Game class for representating a game
data Game = Game {
    gameId :: Integer,       -- ^ gameId the ID in the database of the game
    gameTitle :: String,     -- ^ gameTitle the title of the game
    gameDeveloper :: String, -- ^ gameDeveloper the developer of the game
    gamePublisher :: String  -- ^ gamePublisher the publisher of the game
}

-- | Adds a game to the list of games
addList :: [Game] -> Game -> [Game]
addList [] a = [a]
addList xs a = xs ++ [a]

-- | prints the list to the stdout
printList :: [Game] -> IO([Game])
printList [] = return ( [] )
printList ((Game i t d p):xs) = do putStrLn ( "["++ show i ++"] " ++t ++ " -> " ++ d ++ " -> " ++ p )
                                   ps <- printList xs
                                   return ( [(Game i t d p)] ++ ps)

-- | searches for the title in the second parameter and removes this 
--   item from the list of games
removeList :: [Game] -> String -> [Game]
removeList [] _ = []
removeList ((Game i t d p):xs) title = 
     if (t == title) then
        removeList xs title
     else
        [(Game i t d p)] ++ removeList xs title 

