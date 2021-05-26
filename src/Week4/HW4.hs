{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE LambdaCase #-}
module Week4.HW4 where

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------
-- Define the representation of the polynomial x (coeff = 1, degree = 1)
x :: Num a => Poly a
x = P [0,1]

-- Exercise 2 ----------------------------------------

-- Implement the (==) function

-- This is a bit like list comparison, except that P[a,b,0,...,0] == P[a,b]
-- i.e., we need to ignore trailing zeroes
-- this can be done through pattern matching:
--instance (Num a, Eq a) => Eq (Poly a) where
--    (==) (P (a:as)) (P (b:bs)) = a == b && (P as) == (P bs)
--    (==) (P []) (P (b':bs')) = b' == 0 && (P []) == (P bs')
--    (==) (P(a':as')) (P []) = a' == 0 && (P as') == (P [])
--    (==) (P [])(P[]) = True

-- or, more concisely:
instance (Num a, Eq a) => Eq (Poly a) where
  (==) (P a) (P b) = (dropZeros $ reverse $ a) == (dropZeros $ reverse $ b)

-- helpers
-- Drops leading zeros from numeric list
dropZeros :: (Num a, Eq a) => [a] -> [a]
dropZeros = \case
  a:as -> if a/=0 then (a:as) else dropZeros as
  []   -> []

-- function to reverse list (turns out we can just use `reverse` from Prelude)
-- revList :: [a] -> [a]
-- revList = foldl (\acc x -> x:acc) []

-- Exercise 3 -----------------------------------------

-- implement the show function for polynomials

instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P a) = if a == [] || all (==0) a then "0"
                  else showTuple $ reverse $ toTuple a

-- transforms list of a into tuple of (coeff, ord), and also gets rid of 0s
toTuple :: (Num a, Eq a) => [a] -> [(a,a)]
toTuple lst = helper lst 0
  where
   helper [] _       = []
   helper (a:as) ord = if a == 0 then helper as (ord+1) else [(a,ord)] ++ helper as (ord+1)

--shows (coeff, ord) list in a more readable way 
--I tried to make the syntax prettier but without much sucess :(
showTuple :: (Num a, Eq a, Show a) => [(a,a)] -> String
showTuple []     = ""
showTuple ((a,ord):as) = case ord of
  0  -> show a ++ rest
  1  | a == 1    -> "x" ++ rest 
     | a == (-1) -> "-x" ++ rest
     | otherwise -> show a ++ "x" ++ rest
  _  | a == 1    -> "x^" ++ show ord ++ rest
     | a == (-1) -> "-x^" ++ show ord ++ rest 
     |otherwise  -> show a ++ "x^" ++ show ord ++ rest
  where
   rest = if null as then "" else " + " ++ showTuple as
        
-- Exercise 4 -----------------------------------------

-- define addition for the Poly type
plus :: Num a => Poly a -> Poly a -> Poly a
plus (P a) (P b) = P (addLists a b) 

-- zipWith (+) truncates the sum to match the size of the shortest of the two input lists, which is not what we want
-- so I defined a new function
addLists :: (Num a) => [a] -> [a] -> [a]
addLists (a:as) (b:bs) = (a+b) : addLists as bs
addLists [] b          = b
addLists a []          = a

-- Exercise 5 -----------------------------------------

-- implement polynomial multiplication
times :: Num a => Poly a -> Poly a -> Poly a
times (P a) (P b) = P (timesLists a b)

-- helpers:
-- given lists a and b and an index, multiply every item of b by a!!index (and add leading zeroes accordingly)
timesOneCoeff :: (Num a) => [a] -> [a] -> Int -> [a]
timesOneCoeff a b num     = map (\j -> j * (a!!num)) (replicate num 0 ++ b) 

-- multiply two polynomial lists by executing timesOneCoeff for each index of a, and summing the subresults together
timesLists :: (Num a) => [a] -> [a] -> [a]
timesLists a b = helper a b (length a)
  where helper _ _ 0   = []
        helper l1 l2 num = addLists (timesOneCoeff l1 l2 (num-1)) (helper l1 l2 (num-1))

-- Exercise 6 -----------------------------------------

-- define negate, which negates the input polynomial, and fromInteger, which takes an integer a and returns Poly a
instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate (P a)  = P $ Prelude.negate <$> a
    fromInteger n = P [Prelude.fromInteger n]
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

-- apply a polynomial to a value
applyP :: (Num a, Integral a) => Poly a -> a -> a
applyP (P a) n = sum $ (\(coeff, ord) -> coeff * n^ord) <$> toTuple a

-- Exercise 8 -----------------------------------------

--implement the nderiv (nth derivative) function
class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv n = case n of
                0 -> id
                _ -> deriv . nderiv (n-1)

-- Exercise 9 -----------------------------------------

-- define a function for polynomial differentiation
instance (Num a, Eq a) => Differentiable (Poly a) where
    deriv (P a) = P $ drop 1 $ map (\(coeff, ord) -> coeff * ord) $ toTuple a
