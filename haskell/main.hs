import Game
import SQLite
import Ebay

import Control.Monad 
import Database.HDBC
import Database.HDBC.Sqlite3

-- getArgs
import System.Environment

inputNewGame :: IO(Game)
inputNewGame = 
    do putStrLn "Titel: "
       t <- getLine
       putStrLn "Developer: "
       d <- getLine
       putStrLn "Publisher: "
       p <- getLine
       return (Game 0 t d p)

mainEditGame :: IO(Game)
mainEditGame = 
    do putStrLn "Welches Spiel bearbeiten (int):"
       i <- getLine 
       putStrLn "Titel: "
       t <- getLine
       putStrLn "Developer: "
       d <- getLine
       putStrLn "Publisher: "
       p <- getLine
       return (Game (read i::Integer) t d p)

helptext :: IO()
helptext = 
    do progname <- getProgName
       versiontext
       putStrLn ("\nUsage: "++ progname ++ " (OPTIONS)")
       putStrLn ("Will execute the options iteratively")
       putStrLn ("\nOptions:")
       putStrLn ("  -a | --add           add a game")
       putStrLn ("  -ba | --bulk-add (title) (publisher) (developer)");
       putStrLn ("  -e | --edit          edit a game")
       putStrLn ("  -d | --delete        delete a game from database");
       putStrLn ("  -l | --list          list games")
       putStrLn ("  -p (int) | --price   query prices of a single game")
       putStrLn ("  -pa | --price-all    query prices of all games")
       putStrLn ("  -r | --clear-cache   clear price cache")
       putStrLn ("  -s | --show          prints the ebay cache")
       putStrLn ("  -h | --help          prints this helptext and exits")
       putStrLn ("  -v | --version       prints the version-information and exits");

versiontext :: IO()
versiontext = 
    do putStrLn ("Ebay Game-Retriever")
       putStrLn ("This program helps you retrieving prices for games in your collection.")
       putStrLn ("");
       putStrLn ("Version: 0x46 75 63 6B with Super-Cow-Powers. Really!")
       putStrLn ("Author: Stefan Naumann, 2016")

runAction :: [String] -> Connection -> IO()
runAction [] _ = return ()
runAction (action:args) conn
    | action == "a" || action == "-a" || action == "--add" = 
        do y <- inputNewGame
           insertGame conn y
           runAction args conn
    | action == "ba" || action =="-ba" || action =="--bulk-add" =
        do insertGame conn (Game 0 (args!!0) (args!!1) (args!!2))
           putStrLn ("adding game: "++(args!!0)++" - " ++ (args!!1)++ " -- " ++ (args!!2))
           runAction args conn
    | action == "d" || action == "-d" || action == "--remove" = 
        do printGameTable conn
           putStrLn "Welches Spiel löschen (int):"
           y <- getLine
           deleteGame conn (read y :: Integer)
           runAction args conn
    | action == "l" || action == "-l" || action == "--list" = 
        do printGameTable conn
           runAction args conn
    | action == "e" || action == "-e" || action =="--edit" = 
        do printGameTable conn
           (Game i t d p ) <- mainEditGame 
           game <- queryGame conn i
           editGame conn game t d p
           runAction args conn
    | action == "p" || action == "-p" || action =="--price" = 
        do printGameTable conn
           game <- queryGame conn (read (head args) :: Integer)
           putStrLn $ gameTitle game
           ebayQuery conn game
           runAction args conn
    | action == "s" || action == "-s" || action == "--show" =
        do ebayListCache conn
           runAction args conn
    | action == "pa" || action == "-pa" || action =="--price-all" = 
        do games <- recvGameList conn
           ebayQueryList conn games
           runAction args conn
    | action == "r" || action == "-r" || action == "--clear-cache"  = 
        do ebayClearCache conn
           runAction args conn
    | action == "v" || action == "-v" || action =="--version" =
        do versiontext
           return ()
    | action == "h" || action == "-h" || action =="--help" = 
        do helptext
           return ()
    | otherwise = 
        do putStrLn ("Unknown parameter")
           return ()

main :: IO()
main = do conn <- connectSQLite "game.db"
          args <- getArgs
          gamelist <- recvGameList conn
          if (length args > 0) 
            then do runAction args conn
                    closeSQLite conn
            else do runAction ["-h"] conn
                    closeSQLite conn

