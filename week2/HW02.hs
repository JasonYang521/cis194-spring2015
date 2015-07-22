{-# OPTIONS_GHC -Wall #-}
module HW02 where

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
exactMatches [] [] = 0
exactMatches (x:xs) (y:ys)
	| x==y = 1 + exactMatches xs ys
	| otherwise = exactMatches xs ys
	

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countEachColor :: Code -> Peg -> Int
countEachColor [] _= 0
countEachColor (x:xs) peg
	| x == peg = 1 + countEachColor xs peg
	| otherwise = countEachColor xs peg


countColors :: Code -> [Int]
countColors xs = map (countEachColor xs) colors  

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches xs ys = foldl (+) 0 (zipWith min (countColors xs) (countColors ys))

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove xs ys =  
    let exact = exactMatches xs ys
        noexact = matches xs ys - exact
    in Move ys exact noexact
	

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent (Move c i1 i2) c2 = getMove c2 c == (Move c i1 i2)

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes (Move c i1 i2) codeToFilter = 
    filter (isConsistent(Move c i1 i2)) codeToFilter

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes 1 = [[c] | c <- colors]
allCodes n = [x++y | x <-(allCodes 1), y <- (allCodes (n-1))]

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve = undefined

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
