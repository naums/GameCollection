module SQLite where
import Game

import Control.Monad 
import Database.HDBC 
import Database.HDBC.Sqlite3

import Data.Bool

unpackList :: [[SqlValue]] -> [Game]
unpackList [] = []
unpackList (x:xs) = (packGame x) ++ unpackList xs

packGame :: [SqlValue] -> [Game]
packGame [] = []
packGame (i:x:y:z:xs) = [(Game (fromSql x) (fromSql y) (fromSql z))]

printRows :: [[SqlValue]] -> IO()
printRows [] = return ()
printRows (x:xs) = do printRow x
                      printRows xs

printRow :: [SqlValue] -> IO()
printRow [] = return ()
printRow (i:x:y:z:xs) = putStrLn ( "ID: ["++ fromSql(i) ++"] "++ fromSql(x) ++ " -- " ++ fromSql(y) ++ " -- " ++ fromSql(z) )

connectSQLite :: String -> IO(Connection)
connectSQLite filepath = connectSqlite3 filepath
-- connectMySQL defaultMySQLConnectInfo { mysqlHost = srv, mysqlUser = usr, mysqlPassword = pass, mysqlDatabase=dbname }

closeSQLite :: Connection -> IO()
closeSQLite conn = disconnect conn

queryGameList :: Connection -> IO([[SqlValue]])
queryGameList conn = quickQuery' conn "SELECT id, title, developer, publisher FROM game ORDER BY id ASC" [] 

queryGame :: Connection -> Integer -> IO(Game)
queryGame conn id = do rows <- quickQuery' conn ("SELECT id, title, developer, publisher FROM game WHERE id=" ++ show id) []
                       (g:gs) <- (\ (x:xs) -> return (packGame x)) rows
                       return g

printGameTable :: Connection -> IO()
printGameTable conn = do rows <- queryGameList conn
                         printRows rows

recvGameList :: Connection -> IO([Game])
recvGameList conn = do rows <- queryGameList conn
                       return (unpackList rows)

insertGame :: Connection -> Game -> IO(Game)
insertGame conn (Game t d p) = do x <- run conn ("INSERT INTO game (title, developer, publisher) VALUES ('"++t++"', '"++d++"','"++p++"');") [] 
                                  commit conn
                                  putStrLn (show x ++ " Rows modified")
                                  return (Game t d p)

editGame :: Connection -> Integer -> Game -> String -> String -> String -> IO(Bool)
editGame conn id (Game t d p) title developer publisher = 
    if (i <= 0)
        then return (False)
    else
        do  x <- run conn queryString []
            commit conn
            putStrLn (show x ++ "Rows modified")
            if (x <= 0) 
                then return ( False )
                else return ( True )
    where queryString = "UPDATE game SET " ++ x ++ "WHERE id=" ++ show id
          (x, i) = complQString "title" (complQString "publisher" (complQString "developer" ("", 0) d developer) p publisher) t title

-- complete Query String 
complQString :: String -> (String, Integer) -> String -> String -> (String, Integer)
complQString mode (result, count) orig new
    | orig == new = (result, count)
    | count == 0 = (mode ++ " = '" ++ new ++ "'", count +1)
    | otherwise = (result ++ ", " ++ mode ++ " = '" ++ new ++ "'", count +1)

deleteGame :: Connection -> Integer -> IO(Bool)
deleteGame conn id = do x <- run conn ("DELETE FROM game WHERE id=" ++ show id) []
                        commit conn
                        putStrLn ( show x ++ " Rows modified")
                        if (x <= 0) then return ( False )
                                    else return ( True )
