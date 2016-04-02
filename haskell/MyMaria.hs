module MyMaria where
import Game

import Control.Monad 
import Database.HDBC 
import Database.HDBC.MySQL 

unpackList :: [[SqlValue]] -> [Game]
unpackList [] = []
unpackList (x:xs) = (packGame x) ++ unpackList xs

packGame :: [SqlValue] -> [Game]
packGame [] = []
packGame (x:y:z:xs) = [(Game (fromSql x) (fromSql y) (fromSql z))]

recvGameList :: IO([Game])
recvGameList = do conn <- connectMySQL defaultMySQLConnectInfo { mysqlHost = "127.0.0.1", mysqlUser = "test", mysqlPassword = "t3stpasswort", mysqlDatabase="oettingergames" }
                  rows <- quickQuery' conn "SELECT title, developer, publisher FROM game" [] 
                  return (unpackList rows)

insertGame :: Game -> IO(Game)
insertGame (Game t d p) = do conn <- connectMySQL defaultMySQLConnectInfo { mysqlHost = "127.0.0.1", mysqlUser = "test", mysqlPassword = "t3stpasswort", mysqlDatabase="oettingergames" }
                             x <- run conn ("INSERT INTO game (title, developer, publisher) VALUES ('"++t++"', '"++d++"','"++p++"');") [] 
                             commit conn
                             putStrLn (show x ++ " Rows modified")
                             return (Game t d p)
