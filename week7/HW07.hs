{-# LANGUAGE MonadComprehensions, RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

module HW07 where

import Prelude hiding (mapM)
import Cards

import Control.Applicative ((<*>))
import Control.Arrow (first)
import Control.Monad hiding (mapM, liftM)
import Control.Monad.Random
import Data.Functor
import Data.Monoid
import Data.Vector (Vector, cons, (!), (!?), (//))
import System.Random()

import qualified Data.Vector as V

-- Exercise 1 -----------------------------------------

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f ma = do
  a <- ma
  return $ f a

swapV :: Int -> Int -> Vector a -> Maybe (Vector a)
swapV i j v = (liftM3 $ swapValues i j) (v!?i) (v!?j) (Just v)
  where swapValues a b c d = (// [(a, d), (b, c)])

-- Exercise 2 -----------------------------------------

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f as = sequence (map f as)

getElts :: [Int] -> Vector a -> Maybe [a]
getElts is v = mapM (v!?) is

-- Exercise 3 -----------------------------------------

type Rnd a = Rand StdGen a

randomElt :: Vector a -> Rnd (Maybe a)
randomElt v
  | V.length v == 0 = return Nothing
  | otherwise = fmap (v!?) (getRandomR (0, V.length v - 1))

-- Exercise 4 -----------------------------------------

randomVec :: Random a => Int -> Rnd (Vector a)
randomVec n = V.replicateM n getRandom

randomVecR :: Random a => Int -> (a, a) -> Rnd (Vector a)
randomVecR n r = V.replicateM n (getRandomR r)

-- Exercise 5 -----------------------------------------

shuffleAtPos :: Int -> Vector a -> Rnd (Vector a)
shuffleAtPos i v = do
  let e1 = v ! i
  j <- getRandomR (0, i)
  let e2 = v ! j
  return $ v // [(i, e2), (j, e1)]

shuffle :: Vector a -> Rnd (Vector a)
shuffle v = shuffle' (V.length v - 1) v

shuffle' :: Int -> Vector a -> Rnd (Vector a)
shuffle' 0 v = return v
shuffle' n v = shuffleAtPos n v >>= shuffle' (n-1)

-- Exercise 6 -----------------------------------------

partitionAt :: Ord a => Vector a -> Int -> (Vector a, a, Vector a)
partitionAt v n = (lt, p, gt)
  where p = v!n
        lt = V.ifilter (\i e -> i /= n && e < p) v
        gt = V.ifilter (\i e -> i /= n && e >= p) v

-- Exercise 7 -----------------------------------------

-- Quicksort
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [ y | y <- xs, y < x ]
                   <> (x : quicksort [ y | y <- xs, y >= x ])

qsort :: Ord a => Vector a -> Vector a
qsort v
  | V.length v == 0 = V.empty
  | otherwise = qsort [ y | y <- xs, y < x ]
                      V.++ cons x (qsort [ y | y <- xs, y >= x ])
  where x = v!0
        xs = V.tail v

-- Exercise 8 -----------------------------------------

qsortR :: Ord a => Vector a -> Rnd (Vector a)
qsortR v
  | V.length v == 0 = return V.empty
  | otherwise = do
      pivot <- getRandomR (0, V.length v - 1)
      let (lt, x, gt) = partitionAt v pivot
      l <- qsortR lt
      r <- qsortR gt
      return $ l V.++ cons x r

-- Exercise 9 -----------------------------------------

-- Selection
select :: Ord a => Int -> Vector a -> Rnd (Maybe a)
select i v =
  if i > len
  then return Nothing
  else do
    pivot <- getRandomR (0, len - 1)
    let (l, x, r) = partitionAt v pivot
    case compare i (V.length l) of
      LT -> select i l
      EQ -> return $ Just x
      GT -> select (i - V.length l - 1) r
  where len = V.length v

-- Exercise 10 ----------------------------------------

allCards :: Deck
allCards = [Card l s | s <- suits, l <- labels]

newDeck :: Rnd Deck
newDeck =  shuffle allCards

-- Exercise 11 ----------------------------------------

nextCard :: Deck -> Maybe (Card, Deck)
nextCard d
  | V.length d == 0 = Nothing
  | otherwise = Just (d!0, V.tail d)

-- Exercise 12 ----------------------------------------

getCards :: Int -> Deck -> Maybe ([Card], Deck)
getCards n d = getCards' n (Just ([], d))

consCard :: Maybe (Card, Deck) -> Maybe ([Card], Deck) -> Maybe ([Card], Deck)
consCard a b = (\c d -> (fst c : fst d, snd c)) <$> a <*> b

getCards' :: Int -> Maybe ([Card], Deck) -> Maybe ([Card], Deck)
getCards' n cs
--  | n == 0 = fmap (\(cs, d) -> (reverse cs, d)) cs
  | n == 0 = fmap (first reverse) cs
  | otherwise = do
      let c = cs >>= (nextCard . snd)
      getCards' (n-1) (consCard c cs)

-- Exercise 13 ----------------------------------------

data State = State { money :: Int, deck :: Deck }

repl :: State -> IO ()
repl s@State{..} | money <= 0  = putStrLn "You ran out of money!"
                 | V.null deck = deckEmpty
                 | otherwise   = do
  putStrLn $ "You have \ESC[32m$" ++ show money ++ "\ESC[0m"
  putStrLn "Would you like to play (y/n)?"
  cont <- getLine
  if cont == "n"
  then putStrLn $ "You left the casino with \ESC[32m$"
           ++ show money ++ "\ESC[0m"
  else play
    where deckEmpty = putStrLn $ "The deck is empty. You got \ESC[32m$"
                      ++ show money ++ "\ESC[0m"
          play = do
            putStrLn "How much do you want to bet?"
            amt <- read <$> getLine
            if amt < 1 || amt > money
            then play
            else case getCards 2 deck of
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
