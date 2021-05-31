{-# OPTIONS_GHC -Wall #-}
module Week6.HW6 where

import Data.List
import Data.Functor

-- Exercise 1 -----------------------------------------

-- Write a Fibonacci function
fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n - 2) + fib (n - 1)

-- Define an infinite Fibonnaci sequence
fibs1 :: [Integer]
fibs1 = fib <$> [1..]

-- Exercise 2 -----------------------------------------

-- Define a faster infinite Fibonnaci sequence
fibs2 :: [Integer]
fibs2 = 1 : 1 : zipWith (+) fibs2 (tail fibs2)

-- Exercise 3 -----------------------------------------

data Stream a = Cons a (Stream a)

-- Show instance prints the first 20 elements followed by ellipsis
instance Show a => Show (Stream a) where
    show s = "[" ++ intercalate ", " (map show $ take 64 $ streamToList s)
             ++ ",..."

streamToList :: Stream a -> [a]
streamToList (Cons a str) = a : streamToList str

-- Exercise 4 -----------------------------------------

-- Define a functor instance for streams
instance Functor Stream where
    fmap fun (Cons a str) =  Cons (fun a) (fmap fun str)

-- Exercise 5 -----------------------------------------

-- Write a function that generates a stream containing infinitely many copies of the given element
sRepeat :: a -> Stream a
sRepeat a = Cons a $ sRepeat a

-- Write a function which generates a Stream from a “seed” of type a, which is the first element of the stream, and an “unfolding rule” of type a -> a which specifies how to transform the seed into a new seed, to be used for generating the rest of the stream.
sIterate :: (a -> a) -> a -> Stream a
sIterate f a = Cons a $ sIterate f $ f a

-- Write a function which interleaves the elements from 2 streams
sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (Cons a str1) str2 = Cons a (sInterleave str2 str1)

-- Write a function which takes an Int n and returns a list containing the first n elements in the Stream
sTake :: Int -> Stream a -> [a]
sTake k _  | k <= 0  = []
sTake k (Cons a str) = a : sTake (k-1) str

-- Exercise 6 -----------------------------------------

-- Define the stream of natural numbers 0, 1, 2, ...
nats :: Stream Integer
nats = sIterate (+1) 0

-- Define the stream that corresponds to the ruler function (where the nth element in the string is the largest power of 2 which evenly divides n. We assume the first element corresponds to n = 1)
ruler :: Stream Integer
ruler = helper 0
  where helper n = sInterleave (sRepeat n) (helper $ n+1)

-- Exercise 7 -----------------------------------------

-- | Implementation of C rand
rand :: Int -> Stream Int
rand k = Cons k' (rand k')
  where k' = (1103515245 * k + 12345) `mod` 2147483648

-- Exercise 8 -----------------------------------------

{- Total Memory in use: 207 MiB -}
minMaxSlow :: [Int] -> Maybe (Int, Int)
minMaxSlow [] = Nothing   -- no min or max if there are no elements
minMaxSlow xs = Just (minimum xs, maximum xs)

-- Exercise 9 -----------------------------------------

{- Total Memory in use: 2 MiB -}
minMax :: [Int] -> Maybe (Int, Int)
minMax lst =
  case lst of
    [] -> Nothing
    _ -> helper lst 2147483648 0 -- this is the range of our pseudo-random numbers
      where
        helper [] mini maxi    = Just (mini, maxi)
        helper (x:xs) mini maxi
          |x > maxi  = helper xs mini x
          |x < mini  = helper xs x maxi
          |otherwise = helper xs mini maxi


main :: IO ()
main = print $ minMax $ sTake 1000000 $ rand 7666532

-- Exercise 10 (Optional) -----------------------------

-- Define a type Matrix that represents 2 x 2 matrices of Integers and use it to compute fibonacci numbers quickly
data Matrix = M {x11 :: Integer, x12 :: Integer,
                 x21 :: Integer, x22 :: Integer}

instance Num Matrix where
    (M a b c d) * (M e f g h) = M (a*e + b*g) (a*f + b*h) (c*e + d*g) (c*f + d*h)
-- since the matrix is only 2x2, I just hardcoded the multiplication :P

fibBase :: Matrix
fibBase = M 1 1 1 0

fastFib :: Int -> Integer
fastFib n = x12 $ fibBase ^ n
