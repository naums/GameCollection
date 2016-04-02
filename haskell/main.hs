import Game
import MyMaria

inputNewGame :: IO(Game)
inputNewGame = do putStrLn "Titel: "
                  t <- getLine
                  putStrLn "Developer: "
                  d <- getLine
                  putStrLn "Publisher: "
                  p <- getLine
                  return (Game t d p)

main :: IO([Game])
main = do y <- inputNewGame
          --printList [y]
          insertGame y
          x <- recvGameList
          printList x
          return ([y])

--y <- recvGameList
--          printList y           
---          return ()
