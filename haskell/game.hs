data Game = Game Titel Developer Publisher
type Titel = String
type Developer = String
type Publisher = String

addList :: [Game] -> Game -> [Game]
addList [] a = [a]
addList xs a = xs ++ [a]

printList :: [Game] -> IO([Game])
printList [] = return ( [] )
printList ((Game t d p):xs) = do putStrLn ( t ++ " -> " ++ d ++ " -> " ++ p )
                                 ps <- printList xs
                                 return ( [(Game t d p)] ++ ps)

removeList :: [Game] -> String -> [Game]
removeList [] _ = []
removeList ((Game t d p):xs) title = if (t == title) then
                                        removeList xs title
                                     else
                                        [(Game t d p)] ++ removeList xs title 

sortList :: [Game] -> [Game]
sortList [] = []
sortList [x] = [x]
sortList ((Game ti de pu):xs) = sortList [(Game t d p)| (Game t d p) <- xs, t < ti] ++ [(Game ti de pu)] ++ sortList [(Game t d p)| (Game t d p) <- xs, t > ti]

main :: IO()
main = do printList ( (addList (addList [] (Game "abc" "zumba" "gha")) (Game "Alan Wake" "Remedy" "Remedy Again!")))           
          return ()
