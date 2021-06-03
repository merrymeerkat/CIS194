{-# LANGUAGE MonadComprehensions, RecordWildCards, LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}
module Week7.HW7 where

import Prelude hiding (mapM)
import Week7.Cards

import Control.Monad
import Control.Monad.Random
import Data.Functor
import Data.Monoid
import Data.Vector (Vector, cons, (!), (!?), (//))
import System.Random

import qualified Data.Vector as V

import qualified Data.Maybe as Ma
import Control.Comonad (extract)

-- Exercise 1 -----------------------------------------
-- Define a function that lifts a regular function into a monad and applies it to the argument of that monad
liftM' :: Monad m => (a -> b) -> m a -> m b
liftM' f ma = f <$> ma

-- Alternatively
liftM'' f ma = ma >>= \a -> return $ f a

-- or even 
liftM''' f ma = ma >>= (f >=> return)
-- HLS suggested this >=> operator. Pretty cool :D

-- Define a function that takes in two indices and swaps the elements at those indices in some vector
swapV :: Int -> Int -> Vector a -> Maybe (Vector a)
swapV i j v = do
  let tmp = v !? i
  let change1 = liftM' ((,)i) (v!?j)
  let change2 = liftM' ((,)j) tmp
  let changes = sequence [change1, change2]
  (v V.//) <$> changes
-- This could be shortened but I chose the version above because it is (I think) readable  

-- Exercise 2 -----------------------------------------
-- Implement a function that maps a monadic function across a list
mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' f lst = sequence $ f <$> lst

-- Use mapM to define a function that takes in a list of indices and a vector and returns a list of the elements at those indices in the Maybe monad
getElts :: [Int] -> Vector a -> Maybe [a]
getElts lst v = mapM' (v V.!?) lst

-- Exercise 3 -----------------------------------------

type Rnd a = Rand StdGen a

-- Use the randomness monad to produce a random element of a vector
randomElt :: Vector a -> Rnd (Maybe a)
randomElt v = case length v of
  0 -> return Nothing
  l -> (v V.!?) <$> getRandomR (0, l-1)

-- Exercise 4 -----------------------------------------
-- Write a function that outputs a vector of specified length with random elements
randomVec :: Random a => Int -> Rnd (Vector a)
randomVec l = sequence $ V.generate l (const getRandom)

-- Same as above, but with elements within a range
randomVecR :: Random a => Int -> (a, a) -> Rnd (Vector a)
randomVecR l (a,b) = sequence $ V.generate l (const $ getRandomR (a,b))

-- Exercise 5 -----------------------------------------
--  Implement the Fisher-Yates algorithm to shuffle the elements of a vector

swapV' :: Int -> Int -> Vector a -> Vector a
swapV' a b v = Ma.fromJust $ swapV a b v
--swapV' = Ma.fromJust . swapV 
-- why can we not leave out the parameters, as in the line above?

shuffle :: Vector a -> Rnd (Vector a)
shuffle vt = helper (pure vt) (length vt - 1)
  where helper v 0 = v
        helper v n = helper (swapV' n <$> getRandomR (0, n-1) <*> v) (n - 1)

-- Exercise 6 -----------------------------------------
-- Implement a function that partitions a vector around the element at a given index

-- Appending an element to a vector is O(N), so we first convert the input to a list (to use O(1) cons) and then convert the result back to a vector at the end
partitionAt :: Ord a => Vector a -> Int -> (Vector a, a, Vector a)
partitionAt v i = toTupleV $ helper [] (v!i) [] i (V.toList v)
  where helper lt p gt _ []     = (lt, p, gt)
        helper lt p gt 0 (_:xs) = helper lt p gt (-1) xs --skip the pivot
        helper lt p gt n (x:xs) = if x < p then helper (x:lt) p gt (n-1) xs
                                         else helper lt p (x:gt) (n-1) xs

toTupleV :: ([a], a, [a]) -> (Vector a, a, Vector a)
toTupleV (fs, i, sn) = (V.fromList fs, i, V.fromList sn)


-- Exercise 7 -----------------------------------------
-- Define a naive quicksort for vectors

-- Quicksort (given to us)
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [ y | y <- xs, y < x ]
                   <> (x : quicksort [ y | y <- xs, y >= x ])


qsort :: Ord a => Vector a -> Vector a
qsort v = V.fromList $ quicksort $ V.toList v

-- or, using vectors directly (inefficient)
qsort' :: Ord a => Vector a -> Vector a
qsort' v = case V.uncons v of
   Nothing     -> V.empty
   Just (a,as) -> qsort' [x | x <- as, x < a]
                  <> V.cons a (qsort' [x | x <- as, x >= a])
-- Is there a way to use LambdaCase when we are pattern matching a function
-- of the parameter (e.g. V.uncons v) rather than the parameter itself?

-- Exercise 8 -----------------------------------------
-- Implement randomized quicksort
qsortR :: Ord a => Vector a -> Rnd (Vector a)
qsortR v
  |null v    = return V.empty
  |otherwise = partitionRd v >>= consPartition

-- helper function for further calls of quicksort, and also concatenates the sorted subvectors
consPartition :: Ord a => (Vector a, a, Vector a) -> Rnd (Vector a)
consPartition (v1, p, v2) = liftM2 (<>) (qsortR v1) (liftM' (V.cons p) $ qsortR v2)

-- partitions the vector choosing a random element as pivot
partitionRd :: Ord a => Vector a -> Rnd (Vector a, a, Vector a)
partitionRd v = liftM' (partitionAt v) (getRandomR (0, length v - 1))

-- Exercise 9 -----------------------------------------
-- Implement a randomised algorithm to select the element with rank i in an unsorted array

select :: Ord a => Int -> Vector a -> Rnd (Maybe a)
select i v
  |i >= length v || i < 0 = return Nothing
  |otherwise = partitionRd v >>= selectSubvector i

selectSubvector :: Ord a => Int -> (Vector a, a, Vector a) -> Rnd (Maybe a)
selectSubvector i (v1, p, v2)
  |i < l  = select i v1
  |i == l = pure (Just p)
  |i > l  = select (i-l-1) v2
  where
    l = length v1


-- Exercise 10 ----------------------------------------

-- Implement allCards, which returns a Vector of Cards grouped by suit and arranged from Two to Ace
allCards :: Deck
allCards = [Card l s | l <- toEnum <$> helperVec, s <- V.singleton Spade]
        <> [Card l s | l <- toEnum <$> helperVec, s <- V.singleton Heart]
        <> [Card l s | l <- toEnum <$> helperVec, s <- V.singleton Club]
        <> [Card l s | l <- toEnum <$> helperVec, s <- V.singleton Diamond]

helperVec :: (Num a) => Vector a
helperVec = V.iterateN 13 (+1) 0 -- vector [0,1,...,12]

-- This should return a new Deck that contains all of the cards from allCards, but in a random order
newDeck :: Rnd Deck
newDeck =  shuffle allCards

-- Exercise 11 ----------------------------------------

-- Define a function that takes in a Deck and gives you the head and tail (or Nothing if the Deck is empty)
-- The vector module already has such a function!
nextCard :: Deck -> Maybe (Card, Deck)
nextCard = V.uncons

-- Or, more explicitly:
nextCard' :: Deck -> Maybe (Card, Deck)
nextCard' v
  |l <= 0    = Nothing
  |otherwise = Just (V.head v, V.tail v)
  where l = length v

-- Exercise 12 ----------------------------------------
-- Implement a function that draws n cars from the given Deck

getCards :: Int -> Deck -> Maybe ([Card], Deck)
getCards i d
  |i >= length d || i < 0  = Nothing
  |otherwise  = Just $ helper i d
    where helper 0 v = ([], v)
          helper n v = (V.head v : fst rest, snd rest)
            where rest = helper (n-1) (V.tail v)


-- Exercise 13 ----------------------------------------
-- Simulation of a betting game. This one was given to us!

data State = State { money :: Int, deck :: Deck }

repl :: State -> IO ()
repl s@State{..} | money <= 0  = putStrLn "You ran out of money!"
                 | V.null deck = deckEmpty
                 | otherwise   = do
  putStrLn $ "You have \ESC[32m$" ++ show money ++ "\ESC[0m"
  putStrLn "Would you like to play (y/n)?"
  cont <- getLine
  if cont == "n"
  then putStrLn $ "You left the casino with \ESC[32m$"
           ++ show money ++ "\ESC[0m"
  else play
    where deckEmpty = putStrLn $ "The deck is empty. You got \ESC[32m$"
                      ++ show money ++ "\ESC[0m"
          play = do
            putStrLn "How much do you want to bet?"
            amt <- read <$> getLine
            if amt < 1 || amt > money
            then play
            else do
              case getCards 2 deck of
                Just ([c1, c2], d) -> do
                  putStrLn $ "You got:\n" ++ show c1
                  putStrLn $ "I got:\n" ++ show c2
                  case () of
                    _ | c1 >  c2  -> repl $ State (money + amt) d
                      | c1 <  c2  -> repl $ State (money - amt) d
                      | otherwise -> war s{deck = d} amt
                _ -> deckEmpty
          war (State m d) amt = do
            putStrLn "War!"
            case getCards 6 d of
              Just ([c11, c21, c12, c22, c13, c23], d') -> do
                putStrLn $ "You got\n" ++ ([c11, c12, c13] >>= show)
                putStrLn $ "I got\n" ++ ([c21, c22, c23] >>= show)
                case () of
                  _ | c13 > c23 -> repl $ State (m + amt) d'
                    | c13 < c23 -> repl $ State (m - amt) d'
                    | otherwise -> war (State m d') amt
              _ -> deckEmpty

main :: IO ()
main = evalRandIO newDeck >>= repl . State 100
