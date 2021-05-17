{-# OPTIONS_GHC -Wall #-}
--module week2 where

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int

exactMatches [] _ = 0
exactMatches _ [] = 0
exactMatches (x:xs) (x':xs') 
    |x == x' = 1 + (exactMatches xs xs')
    |otherwise = (exactMatches xs xs')

-- Exercise 2 -----------------------------------------
-- Write a function that returns the number of total matches between the secret code and the guess

-- helper
sumLists :: [Int] -> [Int] -> [Int]
sumLists [] _ = []
sumLists _ [] = []
sumLists (x:xs) (x':xs') = (x + x'):(sumLists xs xs')

--or, with implicit recursion (Jan's suggestion):
sumLists1 :: [Int] -> [Int] -> [Int]
sumLists1 a b = zipWith (+) a b

countColors :: Code -> [Int]

countColors [] = [0,0,0,0,0,0]
countColors (x:xs)
    | x == Red    = sumLists [1,0,0,0,0,0] (countColors xs)
    | x == Green  = sumLists [0,1,0,0,0,0] (countColors xs)
    | x == Blue   = sumLists [0,0,1,0,0,0] (countColors xs)
    | x == Yellow = sumLists [0,0,0,1,0,0] (countColors xs)
    | x == Orange = sumLists [0,0,0,0,1,0] (countColors xs)
    | otherwise   = sumLists [0,0,0,0,0,1] (countColors xs)

sumMin :: [Int] -> [Int] -> Int
sumMin [] _ = 0
sumMin _ [] = 0
sumMin (x:xs) (x':xs') = (min x x') + (sumMin xs xs')

-- or, with implicit recursion and more concisely (Jan's suggestion):
sumMin1 :: [Int] -> [Int] -> Int
sumMin1 a b = sum (zipWith (min) a b)

matches :: Code -> Code -> Int
matches x y = sumMin (countColors x) (countColors y) 

-- Exercise 3 -----------------------------------------
-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove s g = Move (g) (exactMatches s g)  ((matches s g)  - (exactMatches s g))

-- Or:
getMove1 :: Code -> Code -> Move
getMove1 s g = Move g e (m-e) 
        where e = exactMatches s g
              m = matches s g

-- Exercise 4 -----------------------------------------
-- A code is consistent with a Move if the Code could have been the secret that generated that move
-- Write the function isConsistent
isConsistent :: Move -> Code -> Bool

isConsistent (Move c x y) c' 
    |(exactMatches c c') == x && ((matches c c') - (exactMatches c c')) == y = True
    |otherwise                                                               = False
    
-- Or:
isConsistent1 :: Move -> Code -> Bool
isConsistent1 (Move c x y) c'  = e == x && (m - e) == y
    where e = exactMatches c c'
          m = matches c c'

-- Exercise 5 -----------------------------------------
-- Write a function that filters a list of Codes to only contain those that are consistent with a given Move
filterCodes :: Move -> [Code] -> [Code]
filterCodes _ [] = []
filterCodes m (c:cs) = if (isConsistent m c) then (c : filterCodes m cs)
                                             else filterCodes m cs
-- or, more concisely:
filterCodes1 :: Move -> [Code] -> [Code]
filterCodes1 m c = filter (isConsistent m) c

-- Exercise 6 -----------------------------------------
-- Write a function that outputs all possible code permutations of a given length

pegs :: Code
pegs = [Red, Green, Blue, Yellow, Orange, Purple]
baseCodes :: [Code]
--baseCodes = [[Red], [Green], [Blue], [Yellow], [Orange], [Purple]]
baseCodes = [[]]

-- This function takes in a Code of length k and a list of j colors and outputs
-- a list of j codes where each code has length k+1 and originates from the input code plus one color from the color list
addToCode :: Code -> Code -> [Code]
addToCode [] _ = []
addToCode (p:ps) c = [(p:c)] ++ (addToCode ps c)

-- This function takes in a LIST of codes and does the operation described above for each code
addToListCode :: Code -> [Code] -> [Code]
addToListCode p c = concatMap (addToCode p) c

-- Main function: outputs a list of all posible codes of a given length
allCodes :: Int -> [Code]
allCodes 0 = baseCodes
allCodes n = addToListCode pegs (allCodes (n-1))

-- Exercise 7 -----------------------------------------

-- Write a MasterMind solver
solve :: Code -> [Move]

possibilities :: [Code]
possibilities = allCodes 4

-- helper function to reverse list (so that our Move list goes least recent -> most recent guess)
reverseListHelper :: [a] -> [a] -> [a]
reverseListHelper [] acc = acc
reverseListHelper (a:as) acc = reverseListHelper as (a:acc)

reverseList :: [a] -> [a]
reverseList x = reverseListHelper x []

-- another helper
-- takes in a password, a list of possible guesses, and a list of moves (initially empty), 
-- and goes through the possible guesses, filtering out inconsistent ones, until the password is found
-- Output is a list of the moves made until we guess correctly
solveHelper :: Code -> [Code] -> [Move] -> [Move]
solveHelper _ [] m = m
solveHelper c (p:ps) m
    |exactMatches p c == 4 = reverseList((getMove c p) : m)
    |otherwise             = solveHelper c (filterCodes (getMove c p) ps) ((getMove c p) : m)


-- main function uses solveHelper starting with all possible guesses of size 4 and an empty list of Moves
solve c = solveHelper c possibilities []


-----------------------------------
-- sanity check for question 6
----returns length of a list
--length' :: [a] -> Int
--length' [] = 0
--length' (_:xs) = 1 + length' xs

---- power function
--pow :: Integer -> Integer -> Integer
--pow _ 0 = 1
--pow num exp = num * (pow num (exp-1))

---- then we can check whether our output lists have the expected length
---- (we can run the following on ghci)
--length' (allCodes 4) == pow 6 4
--length' (allCodes 7) == pow 6 7
--length' (allCodes 12) == pow 6 12

