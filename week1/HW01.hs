{-# OPTIONS_GHC -Wall #-}
module HW01 where

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit n = mod n 10

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit n = quot n 10

-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits n 
  | n < 1 = []
  | otherwise = [lastDigit n] ++ toRevDigits (dropLastDigit n)
  
  

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x1] = [x1]
doubleEveryOther (x1:x2:xs) = [x1] ++ [2*x2] ++ doubleEveryOther xs

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x1:xs) = lastDigit x1 + dropLastDigit x1 + sumDigits xs 


-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn n = mod (sumDigits (doubleEveryOther (toRevDigits n))) 10 == 0

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 start goal temp = [(start, goal)]
hanoi n start goal temp = hanoi (n-1) start temp goal ++ hanoi 1 start goal temp ++ hanoi (n-1) temp goal start
