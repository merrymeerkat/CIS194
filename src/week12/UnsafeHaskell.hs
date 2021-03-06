-- THE FUNCTIONS IN THIS FILE ARE NOT MINE. These are just notes I took from the CIS194 Week 12 lecture
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Week12.UnsafeHaskell where

import Control.Applicative
import Control.Monad
import Data.ByteString (ByteString)
import Data.IORef
import Data.List
import Data.Word
import Foreign.Marshal.Alloc
import Foreign.Marshal.Array
import Foreign.Ptr
import Foreign.Storable
import System.IO.Unsafe
import Unsafe.Coerce

import qualified Data.ByteString as BS

-- Mutable State

-- In Haskell, mutable states can occur within the IO Monad
-- An IORef is a reference to a mutable cell in memory
-- Here are some basic functions for dealing with IORefs:

-- Initialise an IO Ref
-- newIORef    :: a -> IO (IORef a)

-- Read from an IO Ref
-- readIORef   :: IORef a -> IO a

-- Write to an IO Ref
-- writeIORef  :: IORef a -> a -> IO ()

-- Update the value of an IO Ref
-- modifyIORef :: IORef a -> (a -> a) -> IO ()


-- A few use cases for IO Refs
--
-- Counting the number of times an element appears in a list
count :: Eq a => [a] -> a -> IO Int
count xs x = do
  r <- newIORef 0
  forM_ xs $ \y ->
    when (x == y) $
      modifyIORef r (+1)
  readIORef r

-- or, in wholemeal style:
count2 :: Eq a => [a] -> a -> Int
count2 xs x = length $ findIndices (==x) xs

-- or, an imperative version (like `count` above) that returns Int rather than IO Int
-- this can be done with UnsafePerformIO -> This function should only be used when the IO operation is definitely safe!! using IO Refs locally is usually safe, so here we go...
count3 :: Eq a => [a] -> a -> Int
count3 xs x = unsafePerformIO $ do
  r <- newIORef 0
  forM_ xs $ \y ->
    when (x == y) $
      modifyIORef r (+1)
  readIORef r

-- the code above sucks. It is ugly, uses UnsafePerformIO, and runs 7x slower than the pure version (count2). GHC is optimised for wholemeal style programming

-- Still, in some cases mutable states could actually be useful
-- For example, consider a function that assigns a unique identifier to each node in a tree
--
data Tree a = Node (Tree a) a (Tree a)
            | Empty
              deriving Show

assignIDs :: Tree a -> Tree (Int, a)
assignIDs = snd . assignIDsHelper 0
  where assignIDsHelper id Empty        = (id, Empty)
        assignIDsHelper id (Node l x r) = (id2, Node l' (id1, x) r') 
          where (id1, l') = assignIDsHelper id l
                (id2, r') = assignIDsHelper (id1+1) r

-- It would be more convenient to have a mutable counter:
newCounter :: IO (IO Int)
newCounter = do
  r <- newIORef 0
  return $ do
    v <- readIORef r
    writeIORef r (v + 1)
    return v

printCounts :: IO ()
printCounts = do
  c <- newCounter
  print =<< c
  print =<< c
  print =<< c
-- every time the action c is evaluated, the internal counter is incremented

-- Now we can use this to rewrite the assignIDs function:
assignIDs2 :: Tree a -> IO (Tree (Int, a))
assignIDs2 t = do
  c <- newCounter
  let helper Empty = return Empty
      helper (Node l x r) = do
        l' <- helper l
        id <- c
        r' <- helper r
        return $ Node l' (id, x) r'
  helper t

-- We could also write it using applicative functors, because the value on the counter does not affect the control flow of the program
assignIDs3 :: Tree a -> IO (Tree (Int, a))
assignIDs3 t = do
  c <- newCoutner
  let helper Empty = return Empty
      helper (Node l x r) = Node
                            <$> helper l
                            <*> ( (,x ) <$> c )
                            <*> helper r
  helper t

