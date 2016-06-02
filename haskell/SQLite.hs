{-|
Module      : SQLite
Description : used for database-manipulation and such stuff
Copyright   : (c) Stefan Naumann, 2016
License     : GPL-3
Maintainer  : me@stefannaumann.de
Stability   : experimental

This module incluse functions for retrieving mostly games,
deleting games and managing all the stuff around it, like transferring
the SQLvalues into Game-Objects
-}

module SQLite where
import Game

import Control.Monad 
import Database.HDBC 
import Database.HDBC.Sqlite3

import Data.Bool

-- | executes packGame on a list of database-rows
unpackList :: [[SqlValue]]                  -- ^ list of SQL-rows
           -> [Game]                        -- ^ list of resulting games
unpackList [] = []
unpackList (x:xs) = (packGame x) ++ unpackList xs

-- | creates a Game-object from a list of SQLvalues, ie a SQL-row
packGame :: [SqlValue]                      -- ^ sql-row
         -> [Game]                          -- ^ the resulting game
packGame [] = []
packGame (i:x:y:z:xs) = [(Game (fromSql i) (fromSql x) (fromSql y) (fromSql z))]

-- | prints the rows of a database query to stdout
printRows :: [[SqlValue]]                   -- ^ SQL-rows
          -> IO()
printRows [] = return ()
printRows (x:xs) = do printRow x
                      printRows xs

-- | prints a rows of a game-query
printRow :: [SqlValue]                      -- ^ a sql-row
         -> IO()
printRow [] = return ()
printRow (i:x:y:z:xs) = putStrLn ( "ID: ["++ fromSql(i) ++"] "++ fromSql(x) ++ " -- " ++ fromSql(y) ++ " -- " ++ fromSql(z) )

-- | initiates the database with two tables
initDatabase :: Connection                  -- ^ Database-Connection
             -> IO(Bool)                    -- ^ returns True
initDatabase conn = 
    do quickQuery' conn "CREATE TABLE IF NOT EXISTS game (id INT, title TEXT, developer TEXT, publisher TEXT" []
       quickQuery' conn "CREATE TABLE IF NOT EXISTS game_price (gameId INT, ebayTitle TEXT, ebayURL TEXT, ebayPrice FLOAT, ebayGallery TEXT)" []
       return True;

-- | calls the connect-function
connectSQLite :: String                     -- ^ filepath to the sqlite-file
              -> IO(Connection)             -- ^ returns a database-connection
connectSQLite filepath = connectSqlite3 filepath
-- connectMySQL defaultMySQLConnectInfo { mysqlHost = srv, mysqlUser = usr, mysqlPassword = pass, mysqlDatabase=dbname }

-- | closes the database-connection again
closeSQLite :: Connection                   -- ^ Database-connection to shutdown
            -> IO()
closeSQLite conn = disconnect conn

-- | queries the list of games from the database
queryGameList :: Connection                 -- ^ Database-connection
              -> IO([[SqlValue]])           -- ^ returns the list of resulting sql-rows
queryGameList conn = quickQuery' conn "SELECT id, title, developer, publisher FROM game ORDER BY id ASC" [] 

-- | queries a game from the database, by ID
queryGame :: Connection                     -- ^ Database-connection
          -> Integer                        -- ^ GameID
          -> IO(Game)                       -- ^ returns the resulting game-object
queryGame conn id = do rows <- quickQuery' conn ("SELECT id, title, developer, publisher FROM game WHERE id=" ++ show id) []
                       (g:gs) <- (\ (x:xs) -> return (packGame x)) rows
                       return g

-- | prints the stored games as "table"
printGameTable :: Connection                -- ^ Database-connection
               -> IO()
printGameTable conn = do rows <- queryGameList conn
                         printRows rows

-- | retrieves a list of games from the database and packs it as game-objects
recvGameList :: Connection                  -- ^ Database-connection
             -> IO([Game])                  -- ^ returns the list of games
recvGameList conn = do rows <- queryGameList conn
                       return (unpackList rows)

-- | inserts a game into the database
insertGame :: Connection                    -- ^ Database-connection
           -> Game                          -- ^ Game-object to be saved into the database
           -> IO(Game)                      -- ^ returns the game-object again
insertGame conn (Game i t d p) 
    = do x <- run conn ("INSERT INTO game (title, developer, publisher) VALUES ('"++t++"', '"++d++"','"++p++"');") [] 
         commit conn
         putStrLn (show x ++ " Rows modified")
         return (Game i t d p)

-- | edits a game and updates the database
editGame :: Connection                      -- ^ Database-connection
         -> Game                            -- ^ the game-object with the old data
         -> String                          -- ^ new title
         -> String                          -- ^ new developer
         -> String                          -- ^ new publisher
         -> IO(Bool)                        -- ^ returns True if updated, False otherwiese
editGame conn (Game id t d p) title developer publisher = 
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

-- | complete Query String only of the entry has really changed
complQString :: String                      -- ^ the database-attribute to be changed
             -> (String, Integer)           -- ^ the resulting string with the number of changed attributed
             -> String                      -- ^ the original string
             -> String                      -- ^ the changed string
             -> (String, Integer)           -- ^ returns the resulting string with the number of changed attributes
complQString mode (result, count) orig new
    | orig == new = (result, count)
    | count == 0  = (mode ++ " = '" ++ new ++ "'", count +1)
    | otherwise   = (result ++ ", " ++ mode ++ " = '" ++ new ++ "'", count +1)

-- | deletes a game (found via id) from the database
--   does not delete the price-information about the game!
deleteGame :: Connection                    -- ^ database-connection
           -> Integer                       -- ^ the GameID
           -> IO(Bool)                      -- ^ returns True if the game existed and is now removed
deleteGame conn id = do x <- run conn ("DELETE FROM game WHERE id=" ++ show id) []
                        commit conn
                        putStrLn ( show x ++ " Rows modified")
                        if (x <= 0) then return ( False )
                                    else return ( True )
