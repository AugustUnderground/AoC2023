{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE TypeApplications #-}

module Day07 (run) where

import Data.List (singleton, sort, sortBy, group)

data Card' = J' | Two' | Three' | Four' | Five' | Six' | Seven' | Eight' | Nine' 
           | T' | Q' | K' | A'
          deriving (Ord, Eq, Show)

instance Read Card' where
  readsPrec _ "A" = [(A', "")]
  readsPrec _ "K" = [(K', "")]
  readsPrec _ "Q" = [(Q', "")]
  readsPrec _ "T" = [(T', "")]
  readsPrec _ "9" = [(Nine', "")]
  readsPrec _ "8" = [(Eight', "")]
  readsPrec _ "7" = [(Seven', "")]
  readsPrec _ "6" = [(Six', "")]
  readsPrec _ "5" = [(Five', "")]
  readsPrec _ "4" = [(Four', "")]
  readsPrec _ "3" = [(Three', "")]
  readsPrec _ "2" = [(Two', "")]
  readsPrec _ "J" = [(J', "")]
  readsPrec _  _  = undefined

data Card = Two | Three | Four | Five | Six | Seven | Eight | Nine 
          | T | J | Q | K | A
          deriving (Ord, Eq, Show)

instance Read Card where
  readsPrec _ "A" = [(A, "")]
  readsPrec _ "K" = [(K, "")]
  readsPrec _ "Q" = [(Q, "")]
  readsPrec _ "J" = [(J, "")]
  readsPrec _ "T" = [(T, "")]
  readsPrec _ "9" = [(Nine, "")]
  readsPrec _ "8" = [(Eight, "")]
  readsPrec _ "7" = [(Seven, "")]
  readsPrec _ "6" = [(Six, "")]
  readsPrec _ "5" = [(Five, "")]
  readsPrec _ "4" = [(Four, "")]
  readsPrec _ "3" = [(Three, "")]
  readsPrec _ "2" = [(Two, "")]
  readsPrec _  _  = undefined

data HandType = HighCard | OnePair | TwoPair | ThreeOfAKind
              | FullHouse | FourOfAKind | FiveOfAKind 
              deriving (Ord, Eq, Show)

type Hand  = (HandType, [Card], Int)
type Hand' = (HandType, [Card'], Int)

ht :: [Int] -> HandType
ht [1,1,1,2] = OnePair
ht [1,2,2]   = TwoPair
ht [1,1,3]   = ThreeOfAKind
ht [2,3]     = FullHouse
ht [1,4]     = FourOfAKind
ht [5]       = FiveOfAKind
ht _         = HighCard

handType :: [Card] -> HandType
handType cards = ht values
  where
    cards' = group . sort $ cards
    values = sort $ map length cards'

parse :: [String] -> (HandType, [Card], Int)
parse [cards', bid'] = (hand, cards, bid)
  where
    bid   = read @Int bid'
    cards = map (read @Card . singleton) cards'
    hand  = handType cards
parse _              = undefined

handType' :: [Card'] -> HandType
handType' cards | null js   = ht . sort $ map length cards'
                | j == 5    = FiveOfAKind
                | otherwise = ht . sort . reverse $ (m + j):values'
  where
    (js, cards') = span ((==J') . head) . group . sort $  cards 
    (m:values')  = reverse . sort $ map length cards'
    j            = length $ head js

parse' :: [String] -> (HandType, [Card'], Int)
parse' [cards', bid'] = (hand, cards, bid)
  where
    bid   = read @Int bid'
    cards = map (read @Card' . singleton) cards'
    hand  = handType' cards
parse' _              = undefined

compareHands :: Hand -> Hand -> Ordering
compareHands (hand, cards, _) (hand', cards', _) | ordHands == EQ = ordCards
                                                 | otherwise      = ordHands
  where
    ordHands = compare hand hand'
    ordCards = head . dropWhile (==EQ) $ zipWith compare cards cards'

compareHands' :: Hand' -> Hand' -> Ordering
compareHands' (hand, cards, _) (hand', cards', _) | ordHands == EQ = ordCards
                                                  | otherwise      = ordHands
  where
    ordHands = compare hand hand'
    ordCards = head . dropWhile (==EQ) $ zipWith compare cards cards'

run :: IO ()
run = do
    hands <-  map words . lines <$> readFile "./inputs/day07.txt"
    
    let listOfHands = map parse hands
        ranking     = sortBy compareHands listOfHands
        e1          = sum $ zipWith (\(_,_,bid) rank -> bid * rank) ranking [ 1 .. ]
    
    putStrLn $ "Day 07:\n\tTask 1: " ++ show e1

    let listOfHands' = map parse' hands
        ranking'     = 
            sortBy compareHands' listOfHands'
        e2           = 
            sum $ zipWith (\(_,_,bid) rank -> bid * rank) ranking' [ 1 .. ]

    putStrLn $ "\tTask 2: " ++ show e2
