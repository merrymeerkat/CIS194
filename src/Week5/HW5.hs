{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Week5.HW5 where

import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map
import qualified Data.Bits as D
import qualified Data.ByteString.Lazy.UTF8 as BLU
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.List as DL
import Data.Function (on)

import Week5.Parser
-- Exercise 1 -----------------------------------------
-- Implement a function that takes in the paths to the original and modified files, reads them in as ByteStrings, and then outputs the secret that was encodedd in the image

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret p1 p2 = do
    original <- BS.readFile p1
    encoded <- BS.readFile p2
    return $ BS.pack $ filter (/= 0) $ BS.zipWith D.xor original encoded

-- Exercise 2 -----------------------------------------
-- This function should read in an encrypted file, decrypt it using the key, and then write it back to another file.

decryptWithKey' :: ByteString -> FilePath -> IO ()
decryptWithKey' key outputFile = do
  encryptedFile <- BS.readFile $ outputFile ++ ".enc"
  let fileLength = fromIntegral $ BS.length encryptedFile
  let keyLength = fromIntegral $ BS.length key
  let repeats = div fileLength keyLength
  let repKey = [1..repeats + 1] >> (BLC.unpack key) -- key repeated to fit (at least) the length of the encrypted file
  let res = BS.pack $ BS.zipWith D.xor encryptedFile (BLU.fromString repKey)
  BS.writeFile outputFile res

-- making use of laziness
decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey key outputFile = do
  encryptedFile <- BS.readFile $ outputFile ++ ".enc"  
  let res = BS.pack $ BS.zipWith D.xor encryptedFile (BS.concat $ repeat key)
  BS.writeFile outputFile res


-- Exercise 3 -----------------------------------------
-- Define a function that takes in a path to a JSON file and attempts to parse it as a value of type a.

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile filePath = do
  text <- BS.readFile filePath
  return $ decode text

-- Exercise 4 -----------------------------------------
-- Define a function that takes in a path to the victims list and the path to the transaction data,
-- and returns only the Transactions that occur in the victims list

-- O(N^2)* -> for each transaction, we do an O(N) check to see if it is in victims list
getBadTs_v0 :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs_v0 vi tr = do
  victims <- parseFile vi
  trx <- parseFile tr
  let result = case (trx,victims) of
                    (Nothing, _)     -> Nothing
                    (_, Nothing)     -> Nothing
                    (Just t, Just v) -> Just $ filter (\a -> isIn v (tid a)) t
  return result
-- * or, more specifically, O(VT) where V = num victims, T = num transactions

-- helper
isIn :: Eq a => [a] ->  a -> Bool
isIn [] _ = False
isIn (x:xs) a
  |a == x    = True
  |otherwise = isIn xs a
  
-- O(NlogN) version --sort victims and transactions and then filter out transactions in O(N) time :) 
getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs vi tr = do
  victims <- parseFile vi
  trx <- parseFile tr
  let result = case (trx, victims) of
                    (Nothing, _)     -> Nothing
                    (_, Nothing)     -> Nothing
                    (Just t, Just v) -> Just $ filterTrx (DL.sortBy (compare `on` tid) t) (DL.sort v)
  return result

-- helper (assumes input is sorted)
filterTrx :: [Transaction] -> [TId] -> [Transaction]
filterTrx [] _ = []
filterTrx _ [] = []
filterTrx (tr:trs) (i:ids) 
  |tid tr == i = tr : filterTrx trs ids
  |tid tr < i  = filterTrx trs (i:ids)
  |tid tr > i  = filterTrx trs ids

-- Exercise 5 -----------------------------------------
-- For every name, keep track of how much money that person has gained (or lost) as a result of bad transactions

getFlow :: [Transaction] -> Map String Integer
getFlow []     = Map.empty
getFlow (x:xs) = Map.insertWith (+) (to x) (amount x) $ Map.insertWith (+) (from x) (- amount x) $ getFlow xs
-- the `from` function is (+) (-amount) instead of (-) (amount) to ensure that if the key is not yet present in the map, in which case no function is applied, the value inserted will still be negative

-- Exercise 6 -----------------------------------------
-- The criminal is the person who got the most money: find him/her

getCriminal :: Map String Integer -> String
getCriminal mp = helper 0 "no-one" $ Map.toList mp
  where helper _ name []                 = name
        helper maxi name ((name',val):xs) = if val > maxi then helper val name' xs
                                            else helper maxi name xs

-- Exercise 7 -----------------------------------------
-- Generate a new list of transactions to undo the money transfer

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs mp tids = helper (DL.sort (getPayers mp)) (sortDesc (getPayees mp)) tids 
  where helper [] _ _ = []
        helper _ [] _ = []
        helper _ _ [] = []
        helper (pr@(payer,debit):prs) (pe@(payee,credit):pes) (i:ids)
          |snd pr == 0 = helper prs (pe:pes) (i:ids)
          |snd pe == 0 = helper (pr:prs) pes (i:ids)
          |otherwise   = 
                         let mini = min credit $ abs debit in
                         let newTr = Transaction {from = payee, to = payer, amount = mini, tid = i} in
                         newTr : helper ((payer, debit + mini):prs) ((payee, credit - mini):pes) ids

-- helpers
getPayees :: Map String Integer -> [(String, Integer)]
getPayees mp = filter ((>=0) . snd) $ Map.toList mp

getPayers :: Map String Integer -> [(String, Integer)]
getPayers mp = filter ((<0) . snd) $ Map.toList mp

sortDesc :: [(String, Integer)] -> [(String, Integer)]
sortDesc l = DL.sortBy (\ x y -> if snd x > snd y then GT else LT) l
            
-- Exercise 8 -----------------------------------------
-- Implement a function to write transaction data back to JSON format

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON path trx = 
    BS.writeFile path $ encode trx

-- Exercise 9 -----------------------------------------
-- Putting everything together (the functions below came as part of the homework prompt--I didn't write them)

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
      _ -> doEverything "clues/dog-original.jpg"
                        "clues/dog.jpg"
                        "clues/transactions.json"
                        "clues/victims.json"
                        "clues/new-ids.json"
                        "clues/new-transactions.json"
  putStrLn crim
