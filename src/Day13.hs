{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Day13 (run) where

import Data.List       (elemIndices,transpose)
import Data.List.Split (splitOn)

type Pattern = [[Char]]

smudges :: String -> String -> Int
smudges [] _ = 0
smudges _ [] = 0
smudges (s1:ss1) (s2:ss2) | s1 == s2  = smudges ss1 ss2
                          | otherwise = succ $ smudges ss1 ss2

confirmReflection :: Int -> Pattern -> Int -> Int
confirmReflection s p idx | reflection = idx
                          | otherwise  = 0
  where
    first      = reverse $ take idx p
    second     = drop idx p
    reflection = (== s) . sum $
        zipWith smudges first second

findIndex :: Int -> Pattern -> Int
findIndex s p | s > 0 = sm + ns
              | otherwise = ns
  where
    sm = sum . map (confirmReflection s p . succ) 
       $ elemIndices s . zipWith smudges p . tail $ p
    ns = sum . map (confirmReflection s p . succ)
       $ elemIndices True . zipWith (==) p . tail $ p

findIndices :: Int -> Pattern -> Int
findIndices s p | r > 0     = 100 * r
                | otherwise = c
  where
    r = findIndex s p
    c = findIndex s $ transpose p

run :: IO ()
run = do
    input <- splitOn [""] . lines <$> readFile "./inputs/day13.txt"

    let e1 = sum $ map (findIndices 0) input
        e2 = sum $ map (findIndices 1) input

    putStrLn $ "Day 13:\n\tTask 1: " ++ show e1
    putStrLn $ "\tTask 2: " ++ show e2

    pure ()
