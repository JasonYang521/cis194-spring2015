{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module HW05 where

import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map
import Data.Bits
import Parser

-- Exercise 1 -----------------------------------------

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret filepath1 filepath2 = do 
	resultone <- BS.readFile filepath1
	resulttwo <- BS.readFile filepath2
	let key = BS.pack . filter (/=0) $ BS.zipWith xor resultone resulttwo
	return key

-- Exercise 2 -----------------------------------------

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey key filepath = do	
	ciphered <- BS.readFile(filepath++".enc")
	let deciphered = BS.pack $ BS.zipWith xor ciphered (BS.cycle key)
	BS.writeFile filepath deciphered

-- Exercise 3 -----------------------------------------

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile filePath= do	
	json <- BS.readFile filePath 
	let decodedJson = decode json
	return decodedJson

-- Exercise 4 -----------------------------------------
isVictim :: Maybe [TId] -> Transaction -> Bool
isVictim Nothing _ = False
isVictim (Just ids) t = (tid t) `elem` ids

getTransactions :: Maybe[Transaction] -> [Transaction]
getTransactions Nothing = []
getTransactions (Just transaction) = transaction

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs victimpath transactionpath = do
	victims <- parseFile victimpath
	transactions <- parseFile transactionpath
	let transactionList = getTransactions transactions
	let victimTransactions = filter (isVictim victims) transactionList
	return (Just victimTransactions)
	

-- Exercise 5 -----------------------------------------

getFlow :: [Transaction] -> Map String Integer
getFlow = undefined

-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
getCriminal = undefined

-- Exercise 7 -----------------------------------------

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs = undefined

-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON = undefined

-- Exercise 9 -----------------------------------------

doEverything :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
             -> FilePath -> IO String
doEverything dog1 dog2 trans vict fids out = do
  key <- getSecret dog1 dog2
  decryptWithKey key vict
  mts <- getBadTs vict trans
  case mts of
    Nothing -> error "No Transactions"
    Just ts -> do
      mids <- parseFile fids
      case mids of
        Nothing  -> error "No ids"
        Just ids -> do
          let flow = getFlow ts       
          writeJSON out (undoTs flow ids)
          return (getCriminal flow)

main :: IO ()
main = do
  args <- getArgs
  crim <- 
    case args of
      dog1:dog2:trans:vict:ids:out:_ ->
          doEverything dog1 dog2 trans vict ids out
      _ -> doEverything "dog-original.jpg"
                        "dog.jpg"
                        "transactions.json"
                        "victims.json"
                        "new-ids.json"
                        "new-transactions.json"
  putStrLn crim

