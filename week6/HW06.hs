{-# OPTIONS_GHC -Wall -fno-warn-missing-methods #-}

module HW06 where

import Data.List

-- Exercise 1 -----------------------------------------

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2 -----------------------------------------

fibs2 :: [Integer]
fibs2 = 1 : 1 : zipWith (+) fibs2 (tail fibs2)

-- Exercise 3 -----------------------------------------

data Stream a = Cons a (Stream a)

-- Show instance prints the first 20 elements followed by ellipsis
instance Show a => Show (Stream a) where
    show s = "[" ++ intercalate ", " (map show $ take 10 $ streamToList s)
             ++ ",..."

streamToList :: Stream a -> [a]
streamToList (Cons h t) = h : streamToList t

-- Exercise 4 -----------------------------------------

instance Functor Stream where
    fmap f (Cons h t) = Cons (f h) (fmap f t)

-- a Monoid instance is not possible for Stream because there is nothing to use
-- for the zero

-- Exercise 5 -----------------------------------------

sRepeat :: a -> Stream a
sRepeat x = Cons x (sRepeat x)

sIterate :: (a -> a) -> a -> Stream a
sIterate f x = Cons x (sIterate f (f x))

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (Cons h t) s = Cons h (sInterleave s t)

sTake :: Int -> Stream a -> [a]
sTake 0 _ = []
sTake n (Cons h t) = h : sTake (n-1) t

-- Exercise 6 -----------------------------------------

nats :: Stream Integer
nats = sIterate (+1) 1

ruler :: Stream Integer
ruler = ruler' 0
        where ruler' n = sInterleave (sRepeat n) (ruler' (n+1))

-- Exercise 7 -----------------------------------------

-- | Implementation of C rand
rand :: Int -> Stream Int
rand = sIterate nextrand
  where nextrand n = (1103515245 * n + 12345) `mod` 2147483648

-- Exercise 8 -----------------------------------------

{- Total Memory in use: 119 MB -}
minMaxSlow :: [Int] -> Maybe (Int, Int)
minMaxSlow [] = Nothing   -- no min or max if there are no elements
minMaxSlow xs = Just (minimum xs, maximum xs)

-- Exercise 9 -----------------------------------------

{- Total Memory in use: 107 MB -}
minMax :: [Int] -> Maybe (Int, Int)
minMax = minMax' Nothing
  where minMax' (Just (a,b)) (x:xs) = let c = min a x
                                          d = max b x
                                      in minMax' (Just (c,d)) xs
        minMax' Nothing (x:xs) = minMax' (Just (x,x)) xs
        minMax' x [] = x

{-
-- alternatively
minMax = foldl' minMax' Nothing
  where minMax' (Just (a,b)) x = let c = min a x
                                     d = max b x
                                 in Just (c,d)
        minMax' Nothing x = Just (x,x)
-}

main :: IO ()
main = print $ minMax $ sTake 1000000 $ rand 7666532

-- Exercise 10 ----------------------------------------

data Matrix = Matrix Integer Integer Integer Integer

instance Show Matrix where
  show (Matrix a b c d) = "| " ++ show a ++ " " ++ show b ++ " |\n"
                          ++ "| " ++ show c ++ " " ++ show d ++ " |"

instance Num Matrix where
  fromInteger i = Matrix i 0 0 i
  negate (Matrix a b c d) = Matrix (negate a) (negate b) (negate c) (negate d)
  (Matrix a b c d) + (Matrix e f g h) =
    Matrix (a+e) (b+f) (c+g) (d+h)
  (Matrix a b c d) * (Matrix e f g h) =
    Matrix (a*e + b*g) (a*f + b*h) (c*e + d*g) (c*f + d*h)

-- the Fibonacci matrix
fibMatrix :: Matrix
fibMatrix = Matrix 1 1 1 0

-- get F(n) from a matrix
fibFromMatrix :: Matrix -> Integer
fibFromMatrix (Matrix _ f _ _) = f

fastFib :: Int -> Integer
fastFib n = fibFromMatrix $ fibMatrix^n
