data Game = Game Titel Developer Publisher
type Titel = String
type Developer = String
type Publisher = String

addList :: [Game] -> Game -> [Game]
addList [] a = [a]
addList xs a = xs ++ [a]

printList :: [Game] -> IO([Game])
printList [] = return ( [] )
printList ((Game t d p):xs) = do putStrLn ( t ++"->"++ d ++"->"++ p )
                                 ps <- printList xs
                                 return ( [(Game t d p)] ++ ps)

main :: IO()
main = do printList (addList [] (Game "Alan Wake" "Remedy" "Remedy Again!"))
          return ()
