# Haskell variant

The haskell variant of the gamescollection-program uses a SQLite database to store the saved game-information and prices. It is able to query game-prices from ebay and saves them into the SQLite database. Using command-line-parameters the program can initialize the database, store one or more games in there, query prices and show prices. 

The haskell variant may be used in webserver-configurations, where it may be used as automatic gameprice-querying program in the background. The main application (whether web or regular GUI) can use Pipes for communication and read the output over stdout from the haskell-program to gather information. 

## Requirements

The haskell-programm needs the following haskell-libraries (may be installed with cabal install)
  - [HDBC.SQLite3](https://hackage.haskell.org/package/HDBC-sqlite3)
  - [hsebaysdk](https://hackage.haskell.org/package/hsebaysdk)

Download the .tar.gz-packages and install via

```
tar -xvf hsebaysdk-0.4.0.0.tar.gz
cd hsebaysdk-0.4.0.0
cabal install hsebaysdk.cabal
```

## Usage

See `./main --help` for a list of valid parameters.  
