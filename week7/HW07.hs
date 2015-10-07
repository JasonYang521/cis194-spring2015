{-# LANGUAGE MonadComprehensions, RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
module HW07 where

import Prelude hiding (mapM)
import Cards

import Control.Monad hiding (mapM, liftM)
import Control.Monad.Random
import Data.Functor
import Data.Monoid
import Data.Vector (Vector, cons, (!), (!?), (//))
import System.Random

import qualified Data.Vector as V


-- Exercise 1 -----------------------------------------

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f ma = do
	a <- ma
	return $ f a

liftM' :: Monad m => (a -> b) -> m a -> m b
liftM' f ma = ma >>= \a -> return (f a)
	
swapV :: Int -> Int -> Vector a -> Maybe (Vector a)
swapV i0 i1 v = 
	let swapV' v0 v1 = v//[(i0,v1),(i1,v0)]
	in liftM2 swapV' (v !? i0) (v !? i1) 

-- Exercise 2 -----------------------------------------

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f v = sequence $ map f v

getElts :: [Int] -> Vector a -> Maybe [a]
getElts l v= mapM (v !?) l

-- Exercise 3 -----------------------------------------

type Rnd a = Rand StdGen a

randomElt :: Vector a -> IO (Maybe a)
randomElt v = do
	len <- getRandomR (0, (V.length v) - 1)
	return $ v !? len

-- Exercise 4 -----------------------------------------

randomVec :: Random a => Int -> Rnd (Vector a)
randomVec n = V.replicateM n getRandom

randomVecR :: Random a => Int -> (a, a) -> Rnd (Vector a)
randomVecR n= V.replicateM n . getRandomR

-- Exercise 5 -----------------------------------------

shuffle :: Vector a -> Rnd (Vector a)
shuffle vector = 
    let shuffle' 0 vector' = do return $ vector'
        shuffle' i vector' = do
			j <- getRandomR (0,i)
			v <- shuffle' (i-1)(vector'//[(i,vector' ! j),(j,vector' ! i)])
			return v
	in shuffle' ((V.length vector) -1) vector

-- Exercise 6 -----------------------------------------

partitionAt :: Ord a => Vector a -> Int -> (Vector a, a, Vector a)
partitionAt vector i = ((V.filter (< pivot) vector),pivot, V.ifilter greaterThan vector)
	where 
		pivot = vector ! i
		greaterThan x v = x /= i && pivot <= v

-- Exercise 7 -----------------------------------------

-- Quicksort
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [ y | y <- xs, y < x ]
                   <> (x : quicksort [ y | y <- xs, y >= x ])

qsort :: Ord a => Vector a -> Vector a
qsort vector =
	if V.null vector
	then vector
	else 
		let 
			x = V.head vector
			t = V.tail vector
		in qsort [ y | y <- t, y < x ]
			<> cons x (qsort [ y | y <- t, y >= x ])
-- Exercise 8 -----------------------------------------

qsortR :: Ord a => Vector a -> Rnd (Vector a)
qsortR vector = 
	if V.null vector 
	then do return vector
	else do 
		i <- getRandomR (0, (V.length vector)-1)
		let (smaller, pivot, greater) = (partitionAt vector i)
		greater' <- qsortR greater
		smaller' <- qsortR smaller
		return $ smaller' <> cons pivot greater'
 
-- Exercise 9 -----------------------------------------

-- Selection
select :: Ord a => Int -> Vector a -> Rnd (Maybe a)
select rank vec 
	| V.null vec = do return Nothing
	| rank >= V.length vec = do return Nothing
	| otherwise = do
		i <- getRandomR (0, (V.length vec) -1)
		let(los, pvt, his) = (partitionAt vec i)
		let lo_len = V.length los
		case compare rank lo_len of
			EQ -> return $ Just pvt
			LT -> do
				t <- select rank los
				return $ t
			GT -> do
				t <-select (rank - lo_len -1) his
				return $ t
	

-- Exercise 10 ----------------------------------------

allCards :: Deck
allCards = [Card l s | s <- suits, l <- labels]

newDeck :: Rnd Deck
newDeck = shuffle allCards

-- Exercise 11 ----------------------------------------

nextCard :: Deck -> Maybe (Card, Deck)
nextCard deck
	| V.null deck = Nothing
	| otherwise = Just (V.head deck, V.tail deck)

-- Exercise 12 ----------------------------------------

getCards :: Int -> Deck -> Maybe ([Card], Deck)
getCards num deck
	| num == 0 = Just ([], deck)
	| otherwise = do 
		(nc, deck') <- nextCard deck
		(rc, deck'') <- getCards (num-1) deck'
		return $ ((nc:rc),deck'')

-- Exercise 13 ----------------------------------------

data State = State { money :: Int, deck :: Deck }

repl :: State -> IO ()
repl s@State{..} | money <= 0  = putStrLn "You ran out of money!"
                 | V.null deck = deckEmpty
                 | otherwise   = do
  putStrLn $ "You have $" ++ show money ++ ""
  putStrLn "Would you like to play (y/n)?"
  cont <- getLine
  if cont == "n"
  then putStrLn $ "You left the casino with $"
           ++ show money ++ ""
  else play
    where deckEmpty = putStrLn $ "The deck is empty. You got $"
                      ++ show money ++ ""
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