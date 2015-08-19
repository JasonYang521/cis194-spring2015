{-# OPTIONS_GHC -Wall #-}
module HW06 where

import Data.List
import Data.Functor

-- Exercise 1 -----------------------------------------

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib x = fib (x-1) + fib (x-2)

fibs1 :: [Integer]
fibs1 = map fib (iterate (+1) 0)   

-- Exercise 2 -----------------------------------------

fibs2 :: [Integer]
fibs2 = 1 : 1 : zipWith (+) (tail fibs2) fibs2 

-- Exercise 3 -----------------------------------------

data Stream a = Cons a (Stream a)

-- Show instance prints the first 20 elements followed by ellipsis
instance Show a => Show (Stream a) where
    show s = "[" ++ intercalate ", " (map show $ take 10 $ streamToList s)
             ++ ",..."

streamToList :: Stream a -> [a]
streamToList (Cons a (b)) = (a : streamToList b) 

-- Exercise 4 -----------------------------------------

instance Functor Stream where
    fmap function (Cons a (b)) = Cons (function a) (fmap function b)

-- Exercise 5 -----------------------------------------

sRepeat :: a -> Stream a
sRepeat value = Cons value (sRepeat value)

sIterate :: (a -> a) -> a -> Stream a
sIterate function1 seed = Cons seed (sIterate function1 (function1 seed))

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (Cons a s1) s2 =  Cons a (sInterleave s2 s1)

sTake :: Int -> Stream a -> [a]
sTake n stream1= take n (streamToList stream1) 

-- Exercise 6 -----------------------------------------

nats :: Stream Integer
nats = sIterate (+1) 0

ruler :: Stream Integer
ruler = undefined

-- Exercise 7 -----------------------------------------
crand:: Int -> Int
crand int = (1103515245 * int + 12345) `mod` 2147483648

-- | Implementation of C rand
rand :: Int -> Stream Int
rand integer = sIterate crand integer
-- Exercise 8 -----------------------------------------

{- Total Memory in use: 235 MB -}
minMaxSlow :: [Int] -> Maybe (Int, Int)
minMaxSlow [] = Nothing   -- no min or max if there are no elements
minMaxSlow xs = Just (minimum xs, maximum xs)

-- Exercise 9 -----------------------------------------



{- Total Memory in use: 68 MB -}
minMax :: [Int] -> Maybe (Int, Int)
minMax [] = Nothing
minMax (x:xs) = Just(minFunc xs x, maxFunc xs x)

minFunc :: [Int] -> Int -> Int
minFunc (x:xs) i
	| x < i = minFunc xs x
	| otherwise = minFunc xs i
minFunc [] i = i
	
maxFunc :: [Int] -> Int -> Int
maxFunc (x:xs) i
	| x > i = maxFunc xs x
	| otherwise = maxFunc xs i	
maxFunc [] i = i

main :: IO ()
main = print $ minMax $ sTake 1000000 $ rand 7666532

-- Exercise 10 ----------------------------------------

fastFib :: Int -> Integer
fastFib = undefined
