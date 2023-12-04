{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Day04 (run) where

import           Data.Maybe      (fromJust)
import           Data.List       (elemIndex)
import           Data.Set        (Set)
import qualified Data.Set as S
import           Data.Map        (Map, (!))
import qualified Data.Map as M

splitCards :: String -> (Set Int, Set Int)
splitCards l = (wSet, gSet)
    where
      l' = drop 1 $ dropWhile (/=':') l
      bar = fromJust $ elemIndex '|' l'
      wSet = S.fromList . map (read @Int) . words $ take (bar - 1) l'
      gSet = S.fromList . map (read @Int) . words $ drop (bar + 1) l'

score :: Set Int -> Int
score matches | S.null matches = 0
              | otherwise      = product $ take (S.size matches) worth
  where
    worth = 1 : repeat 2

copies :: Map Int Int -> Int -> Int -> Map Int Int
copies cpy' cdn cpy = foldr (M.adjust (+num)) cpy' [cdn + 1 .. cdn + cpy]
  where
    num = cpy' ! cdn

run :: IO ()
run = do
    cards' <- lines <$> readFile "./inputs/day04.txt"

    let cards   = map splitCards cards'
        matches = map (uncurry S.intersection) cards
        e1      = sum . map score $ matches

    putStrLn $ "Day 04:\n\tTask 1: " ++ show e1

    let numMatches = M.fromList . zip [1 ..] $ map S.size matches
        copies'    = M.fromList . map (,1) $ M.keys numMatches
        e2         = M.foldl (+) 0 $ M.foldlWithKey copies copies' numMatches

    putStrLn $ "\tTask 2: " ++ show e2
