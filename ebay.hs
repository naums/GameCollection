-- crude experiments with the ebay API in Haskell

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Data.Monoid ((<>))
import Web.Ebay
--import Data.Text.Internal
import Data.Text

import GHC.Base
import GHC.List

main :: IO ()
main = simpleSearchWithVerb config searchRequest
   >>= printAvg
  where
    condition = "Used"
    keywords = "Prince of Persia"
    config = defaultEbayConfig { ebDomain = "svcs.ebay.com"
                                 -- ^ Use `svcs.sandbox.ebay.com` when
                                 --   connecting to sandbox.
                               , ebAppId = "StefanNa-GameAccu-SBX-33890c490-77cdf594"
                               , ebDebug = False
                               , ebSiteId = "EBAY-US"
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


printAvg :: Maybe SearchResponse -> IO ()
printAvg a = do y <- averageCurrentPrice a
                putStrLn ("Hello!" ++ show y);


bcd :: [SearchItem] -> IO()
bcd [] = return ()
bcd (x:xs) = do bcd xs
                putStrLn (unpack (searchItemViewItemUrl x))

-- | Calculate the average price from the listings within a
-- 'SearchResponse'.
averageCurrentPrice :: Maybe SearchResponse -> IO(Double)
averageCurrentPrice sr = case sr of
    Nothing -> return(0.0)
    Just (SearchResponse _ SearchResult{..}) ->
        let price SellingStatus{..} = sellingStatusConvertedCurrentPrice
            f SearchItem{..} = (+) (price searchItemSellingStatus)
            total = GHC.Base.foldr f 0.0 searchResultItems
        in do bcd searchResultItems
              return (total / fromIntegral (GHC.List.length searchResultItems))
