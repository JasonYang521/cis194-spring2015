module Cards ( Deck(..)
             , Card(..)
             , Suit(..)
             , Label(..)
             , suits
             , labels) where

import Data.Vector (Vector)
import qualified Data.Vector as V

type Deck = Vector Card

-- The Card data type
data Card  = Card Label Suit

instance Eq Card where
    Card x _ == Card y _ = x == y

instance Ord Card where
    compare (Card x _) (Card y _) = compare x y

instance Show Card where
    show (Card l s) = show l ++ show s

data Suit  = Spade | Heart | Club | Diamond
             deriving (Eq)

instance Show Suit where
    show Spade   = "S"
    show Heart   = "H"
    show Club    = "C"
    show Diamond = "D"

data Label = Two   | Three | Four  | Five  | Six   | Seven
           | Eight | Nine  | Ten   | Jack  | Queen | King  | Ace
             deriving (Eq, Ord, Enum)

instance Show Label where
    show l = "-" ++ show' l ++ space  ++ "--"
        where
          space | l /= Ten  = " "
                | otherwise = ""
          show' Jack  = "J"
          show' Queen = "Q"
          show' King  = "K"
          show' Ace   = "A"
          show' lbl   = show $ fromEnum lbl + 2

suits :: Vector Suit
suits = V.fromList [Spade, Heart, Club, Diamond]

labels :: Vector Label
labels = V.fromList [Two .. Ace]
