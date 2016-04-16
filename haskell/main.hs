import Game
import SQLite

import Control.Monad 
import Database.HDBC
import Database.HDBC.Sqlite3

inputNewGame :: IO(Game)
inputNewGame = do putStrLn "Titel: "
                  t <- getLine
                  putStrLn "Developer: "
                  d <- getLine
                  putStrLn "Publisher: "
                  p <- getLine
                  return (Game t d p)

editGame :: IO()
editGame = return ()

--deleteGame :: IO()
--deleteGame = return ()

runAction :: String -> Connection -> IO()
runAction action conn
    | action == "a" = do y <- inputNewGame
                         insertGame conn y
                         return ()
    | action == "d" = do printGameTable conn
                         putStrLn "Welches Spiel löschen (int):"
                         y <- getLine
                         deleteGame conn (read y :: Integer)
                         return ()
    | action == "l" = do printGameTable conn
                         return ()
    | action == "e" = do return ()
    | otherwise = return ()

main :: IO()
main = do conn <- connectSQLite "game.db"
          putStrLn ("d <- delete\na <- add\ne <- edit\nl <- list")
          action <- getLine
          gamelist <- recvGameList conn
          runAction action conn
          closeSQLite conn
