-- crude experiments with the ebay API in Haskell

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
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

ebayClearCache :: Connection -> IO(Integer)
ebayClearCache conn = 
    do x <- run conn ("DELETE FROM game_price") []
       commit conn
       putStrLn ("Deleted " ++ (show x) ++ " rows");
       return x

ebayQueryList :: Connection -> [Game] -> IO()
ebayQueryList _ [] = return ()
ebayQueryList conn (x:xs) = 
    do ebayQuery conn x
       ebayQueryList conn xs

ebayQuery :: Connection -> Game -> IO ()
ebayQuery conn (Game gameId title _ _) = 
    do searchResult <- simpleSearchWithVerb config searchRequest
       y <- ebayStoreResults conn gameId searchResult
       putStrLn (show y ++ " Entries saved")
  where
    condition = "New"
    keywords = pack(title)
    config = defaultEbayConfig { ebDomain = "svcs.sandbox.ebay.com"
                                 -- ^ Use `svcs.sandbox.ebay.com` when
                                 --   connecting to sandbox.
                               , ebAppId = "StefanNa-GameAccu-SBX-33890c490-77cdf594"
                               , ebDebug = False
                               , ebSiteId = "EBAY-DE" -- "EBAY-DE"
                               , ebHttps = False
                               }
    -- Find items by keywords.
    search = Search { searchKeywords = keywords
                    , searchOutputSelector = Just PictureURLLarge
                    , searchSortOrder = Nothing
                    , searchItemFilter = [ ItemFilter ("Condition", condition) ]
                    , searchAffiliateInfo = Nothing
                    , searchProductId = Nothing
                    }

    searchRequest = SearchRequest FindItemsByKeywords search

ebayPrintList :: [SqlValue] -> IO()
ebayPrintList [] = return ()
ebayPrintList (a:b:c:d:e:xs) = 
    do putStrLn ((fromSql a) ++ ", " ++ fromSql b ++ ", " ++ fromSql c ++ ", " ++ fromSql d ++ ", " ++ (fromSql e))
       ebayPrintList xs
    
ebayPrintListRow :: [[SqlValue]] -> IO()
ebayPrintListRow [] = return ()
ebayPrintListRow (x:xs) = 
    do ebayPrintList x
       ebayPrintListRow xs

ebayListCache :: Connection -> IO()
ebayListCache conn = 
    do x <- quickQuery' conn "SELECT ebayPrice, ebayURL, ebayGallery, ebayTitle, gameId FROM game_price ORDER BY ebayPrice ASC" []
       ebayPrintListRow x

ebayStoreResults :: Connection -> Integer -> Maybe SearchResponse -> IO(Integer)
ebayStoreResults conn gameId sr = case sr of    
    Nothing -> return(0)
    Just (SearchResponse _ SearchResult{..}) ->
        do ebayStoreSingleResult conn (searchResultItems) 0 gameId
              
ebayStoreSingleResult :: Connection -> [SearchItem] -> Integer -> Integer -> IO(Integer)
ebayStoreSingleResult _ [] count _ = return count
ebayStoreSingleResult conn (x:xs) count gameId = 
    do x <- run conn ("INSERT INTO game_price (gameId, ebayTitle, ebayURL, ebayPrice, ebayGallery) VALUES (" 
                       ++ show gameId ++ ", '"
                       ++ unpack (searchItemTitle x) ++ "', '" 
                       ++ unpack (searchItemViewItemUrl x) ++ "', "
                       ++ show (sellingStatusConvertedCurrentPrice (searchItemSellingStatus x)) ++ ",'"
                       ++ galleryUrl ++ "')") [] 
       commit conn
       putStrLn (show x ++ " Rows modified")
       ebayStoreSingleResult conn xs (count+1) gameId
       where galleryUrl = maybe "" unpack (searchItemGalleryUrl x)
