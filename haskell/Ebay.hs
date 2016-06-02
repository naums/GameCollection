{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-
Module      : Ebay
Description : functions for ebay-querying and price-saving
Copyright   : (c) Stefan Naumann, 2016
License     : GPL-3
Maintainer  : me@stefannaumann.de
Stability   : experimental
Portability : 

contains functions for retrieving prices for games from ebay, 
searching by title of the game. Also: saving and retrieving 
the prices from the local database
-}
module Ebay where

import SQLite
import Game

import Control.Monad 
import Database.HDBC 
import Database.HDBC.Sqlite3

import Data.Monoid ((<>))
import Web.Ebay
import Data.Text

import GHC.Base
import GHC.List

-- | empties the database for saving game-prices
ebayClearCache :: Connection                -- ^ Database-connection
               -> IO(Integer)               -- ^ returns how many rows were affected
ebayClearCache conn = 
    do x <- run conn ("DELETE FROM game_price") []
       commit conn
       putStrLn ("Deleted " ++ (show x) ++ " rows");
       return x

-- | queries ebay for a list of games
ebayQueryList :: Connection                 -- ^ Database-Connection
              -> [Game]                     -- ^ list of games to query
              -> IO()
ebayQueryList _ [] = return ()
ebayQueryList conn (x:xs) = 
    do ebayQuery conn x
       ebayQueryList conn xs

-- | queries ebay for a specific games
ebayQuery :: Connection                     -- ^ Database-Connection
          -> Game                           -- ^ the game for querying
          -> IO ()
ebayQuery conn (Game gameId title _ _) = 
    do searchResult <- simpleSearchWithVerb config searchRequest
       y <- ebayStoreResults conn gameId searchResult
       putStrLn (show y ++ " Entries saved")
  where
    condition = "New"
    keywords = pack(title)
    config = defaultEbayConfig { 
        ebDomain = "svcs.sandbox.ebay.com"
         --  Use `svcs.sandbox.ebay.com` when
         --   connecting to sandbox.
       , ebAppId = "StefanNa-GameAccu-SBX-33890c490-77cdf594"
       , ebDebug = False
       , ebSiteId = "EBAY-DE" -- "EBAY-DE"
       , ebHttps = False
       }
    -- Find items by keywords.
    search = Search { 
          searchKeywords = keywords
        , searchOutputSelector = Just PictureURLLarge
        , searchSortOrder = Nothing
        , searchItemFilter = [ ItemFilter ("Condition", condition) ]
        , searchAffiliateInfo = Nothing
        , searchProductId = Nothing
        }

    searchRequest = SearchRequest FindItemsByKeywords search

-- | prints a list of Sqlvalues to stdout;
--   used for printing queried prices
ebayPrintList :: [SqlValue]                 -- ^ a database-row
              -> IO()
ebayPrintList [] = return ()
ebayPrintList (a:b:c:d:e:xs) = 
    do putStrLn ((fromSql a) ++ ", " ++ fromSql b ++ ", " ++ fromSql c ++ ", " ++ fromSql d ++ ", " ++ (fromSql e))
       ebayPrintList xs

-- | prints a row of the prices-table to stdout
ebayPrintListRow :: [[SqlValue]]            -- ^ list of database-rows
                 -> IO()
ebayPrintListRow [] = return ()
ebayPrintListRow (x:xs) = 
    do ebayPrintList x
       ebayPrintListRow xs

-- | queries the already stored prices and prints the results to stdout
ebayListCache :: Connection                 -- ^ Database-Conenction
              -> IO()
ebayListCache conn = 
    do x <- quickQuery' conn "SELECT ebayPrice, ebayURL, ebayGallery, ebayTitle, gameId FROM game_price ORDER BY gameId ASC" []
       ebayPrintListRow x

-- | creates a json-"object" as string and returns it
--   from a queried game-price off the database
ebayPrintResultListItem :: [SqlValue]       -- ^ the queried data, i.e. one row of it
                        -> String           -- ^ a json-object as string representing the game-data
ebayPrintResultListItem [] = ""
ebayPrintResultListItem (price:url:gal:etitle:gameTitle:ys) = "{ \"gameTitle\" : \"" ++ fromSql gameTitle ++ "\", \"ebayPrice\": \"" ++ fromSql price ++ "€\", \"ebayTitle\" : \"" ++ fromSql etitle ++ "\", \"ebayGallery\": \""++ fromSql gal ++"\", \"ebayURL\": \"" ++ fromSql url ++ "\" }" 

-- | creates a JSON-array-content from a list of queried prices
ebayPrintResultList :: [[SqlValue]]         -- ^ the queried rows
                    -> Int                  -- ^ the number of already created object (for determination if there has to be an "," beforehand)
                    -> String               -- ^ a json-array (without the [..]-brackets) representing all the queried rows
ebayPrintResultList [] _ = ""
ebayPrintResultList (x:xs) count
    | count <= 0 = ebayPrintResultListItem x ++ ebayPrintResultList xs 1
    | otherwise = "," ++ ebayPrintResultListItem x ++ ebayPrintResultList xs 1

-- | queries the prices of a game (by name) and prints these as JSON-array to stdout
ebayPrintResults :: Connection              -- ^ Database-Connection
                 -> String                  -- ^ the name of the game
                 -> IO()                    
ebayPrintResults _ [] = return ()
ebayPrintResults conn name = 
    do x <- quickQuery' conn ("SELECT ebayPrice, ebayURL, ebayGallery, ebayTitle, game.title FROM game_price JOIN game ON game.id = game_price.gameId WHERE game.title = '" ++ name ++ "'") []
       putStrLn ( "[" ++ ebayPrintResultList x 0 ++ "]" )

-- | gets the response of ebay and will use the function ebayStoreSingleResult for every resulting object
ebayStoreResults :: Connection              -- ^ Database-Connection
                 -> Integer                 -- ^ The ID of the queried game
                 -> Maybe SearchResponse    -- ^ maybe a searchResult of ebay, may also be nothing
                 -> IO(Integer)             -- ^ returns the number of inserted objects
ebayStoreResults conn gameId sr = case sr of    
    Nothing -> return(0)
    Just (SearchResponse _ SearchResult{..}) ->
        do ebayStoreSingleResult conn (searchResultItems) 0 gameId

-- | saves a single game inclusive price into the price-table of the database
ebayStoreSingleResult :: Connection         -- ^ Database-Connection
                      -> [SearchItem]       -- ^ list of resulting searchItems of ebay
                      -> Integer            -- ^ the number of already processed elements (will be returned at the end)
                      -> Integer            -- ^ ID of the queried game
                      -> IO(Integer)        -- ^ number of stored game-prices
ebayStoreSingleResult _ [] count _ = return count
ebayStoreSingleResult conn (x:xs) count gameId = 
    do x <- run conn (
           "INSERT INTO game_price (gameId, ebayTitle, ebayURL, ebayPrice, ebayGallery) VALUES (" 
           ++ show gameId ++ ", '"
           ++ unpack (searchItemTitle x) ++ "', '" 
           ++ unpack (searchItemViewItemUrl x) ++ "', "
           ++ show (sellingStatusConvertedCurrentPrice (searchItemSellingStatus x)) ++ ",'"
           ++ galleryUrl ++ "')") [] 
       commit conn
       putStrLn (show x ++ " Rows modified")
       ebayStoreSingleResult conn xs (count+1) gameId
       where galleryUrl = maybe "" unpack (searchItemGalleryUrl x)
