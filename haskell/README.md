# Haskell variant

The haskell variant of the gamescollection-program uses a SQLite database to store the saved game-information and prices. It is able to query game-prices from ebay and saves them into the SQLite database. Using command-line-parameters the program can initialize the database, store one or more games in there, query prices and show prices. 

The haskell variant may be used in webserver-configurations, where it may be used as automatic gameprice-querying program in the background. The main application (whether web or regular GUI) can use Pipes for communication and read the output over stdout from the haskell-program to gather information. 

## Usage

See ./main --help for a list of valid parameters.  
