{-# OPTIONS_GHC -Wall #-}
module Week1.HW1 where

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit n = mod n 10

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit n = div n 10

-- Exercise 2 -----------------------------------------
-- Write a function that breaks apart a number into a list of its digits in reverse order

toRevDigits :: Integer -> [Integer]
toRevDigits num = helper num []
  where helper n acc = if n <= 0 then revList' acc
                                 else helper (dropLastDigit $ n) ((lastDigit n) :acc)

-- helper: a function that reverses lists
revList :: [a] -> [a]
revList l = helper l [] 
  where helper x acc = case x of []      -> acc
                                 (x':xs) -> helper xs (x':acc)

-- or, more concisely
revList' :: [a] -> [a]
revList' l = foldl (\acc x -> x:acc) [] l

-- I also found this version on stackOverflow. I was a bit confused at first as to why foldl can simply take 2 arguments here. Now I think it's because the compiler `knows` that if revList'' takes in a list, its foldl definition must also take in a list, so we can just let the l be implicit (?)
revList'' :: [a] -> [a]
revList'' = foldl (\acc x -> x:acc) []

-- Exercise 3 -----------------------------------------
-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther l = doubleHelper l False

doubleHelper :: [Integer] -> Bool -> [Integer]
doubleHelper l double = case l of
                          (x:xs) -> if double then 2*x:(doubleHelper xs False) else x:(doubleHelper xs True)
                          []     -> []

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
-- concise :D
sumDigits :: [Integer] -> Integer
sumDigits l = sum $ sum <$> toRevDigits <$> l
-- the idea is: you turn each integer into a list of its digits, then get the sum of each of these lists, and finally sum the subresults together 

-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn num = (lastDigit $ sumDigits $ doubleEveryOther $ toRevDigits num) == 0

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a

-- or
hanoi' :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi' n a b c = if n == 0 then []
                else hanoi' (n-1) a c b ++ [(a,b)] ++ hanoi' (n-1) c b a

-- which is better?
