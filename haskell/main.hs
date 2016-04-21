import Game
import SQLite
import Ebay

import Control.Monad 
import Database.HDBC
import Database.HDBC.Sqlite3

-- getArgs
import System.Environment

inputNewGame :: IO(Game)
inputNewGame = do putStrLn "Titel: "
                  t <- getLine
                  putStrLn "Developer: "
                  d <- getLine
                  putStrLn "Publisher: "
                  p <- getLine
                  return (Game 0 t d p)

mainEditGame :: IO()
mainEditGame = return ()

--deleteGame :: IO()
--deleteGame = return ()

runAction :: [String] -> Connection -> IO()
runAction [] _ = return ()
runAction (action:args) conn
    | action == "a" || action == "-a" = 
        do y <- inputNewGame
           insertGame conn y
           runAction args conn
    | action == "d" || action == "-d" = 
        do printGameTable conn
           putStrLn "Welches Spiel löschen (int):"
           y <- getLine
           deleteGame conn (read y :: Integer)
           runAction args conn
    | action == "l" || action == "-l" = 
        do printGameTable conn
           runAction args conn
    | action == "e" || action == "-e" = 
        do printGameTable conn
           putStrLn "Welches Spiel bearbeiten (int):"
           i <- getLine 
           putStrLn "Titel: "
           t <- getLine
           putStrLn "Developer: "
           d <- getLine
           putStrLn "Publisher: "
           p <- getLine
           game <- queryGame conn (read i :: Integer)
           editGame conn game t d p
           runAction args conn
    | action == "p" || action == "-p" = 
        do printGameTable conn
           game <- queryGame conn (read (head args) :: Integer)
           putStrLn $ gameTitle game
           ebayQuery conn game
           runAction args conn
    | action == "r" || action == "-r"  = 
        do ebayClearCache conn
           runAction args conn
    | action == "h" || action == "-h" || action =="--help" = 
        do putStrLn ("Usage:\n\t-d -> delete\n\t-a -> add\n\t-e -> edit\n\t-l -> list\n\t-p (int) -> query single price\n\t-r -> clear price cache\n\t-pa -> query all prices\n\t-h | --help -> Show this helptext")
           return ()
    | otherwise = 
        do putStrLn ("Unknown parameter")
           return ()

main :: IO()
main = do conn <- connectSQLite "game.db"
          args <- getArgs
          gamelist <- recvGameList conn
          runAction args conn
          closeSQLite conn
