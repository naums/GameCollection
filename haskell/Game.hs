module Game where

-- Game class for representating a game
-- \member gameId the ID in the database of the game
-- \member gameTitle the title of the game
-- \member gameDeveloper the developer of the game
-- \member gamePublisher the publisher of the game
data Game = Game {
    gameId :: Integer, 
    gameTitle :: String, 
    gameDeveloper :: String, 
    gamePublisher :: String
}


addList :: [Game] -> Game -> [Game]
addList [] a = [a]
addList xs a = xs ++ [a]

printList :: [Game] -> IO([Game])
printList [] = return ( [] )
printList ((Game i t d p):xs) = do putStrLn ( "["++ show i ++"] " ++t ++ " -> " ++ d ++ " -> " ++ p )
                                   ps <- printList xs
                                   return ( [(Game i t d p)] ++ ps)

removeList :: [Game] -> String -> [Game]
removeList [] _ = []
removeList ((Game i t d p):xs) title = 
     if (t == title) then
        removeList xs title
     else
        [(Game i t d p)] ++ removeList xs title 

--sortList :: [Game] -> [Game]
--sortList [] = []
--sortList [x] = [x]
--sortList ((Game id ti de pu):xs) = sortList [m| m <- xs, t <- (gameTitle m), compare t ti == LT] ++ [(Game id ti de pu)] ++ sortList [m| m <- xs, t <- (gameTitle m), compare t ti == GT]


