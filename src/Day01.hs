{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Day01 (run) where

import Data.Char                (isDigit)

word2digit :: String -> String
word2digit s | take 4 s == "nine"  = '9' : tail s
             | take 5 s == "eight" = '8' : tail s
             | take 5 s == "seven" = '7' : tail s
             | take 3 s == "six"   = '6' : tail s
             | take 4 s == "five"  = '5' : tail s
             | take 4 s == "four"  = '4' : tail s
             | take 5 s == "three" = '3' : tail s
             | take 3 s == "two"   = '2' : tail s
             | take 3 s == "one"   = '1' : tail s
             | otherwise           =            s

w2d :: String -> String -> String
w2d s' ""  = s'
w2d s'  s  = w2d (s' ++ f) l
  where
    (f,l) = splitAt 1 $ word2digit s

run :: IO ()
run = do
    ls  <- lines <$> readFile "./inputs/day01.txt"

    let ds = map (filter isDigit) ls
        f  = map head ds
        l  = map last ds
        e1 = sum $ zipWith (\a b -> read @Int [a,b]) f l
    putStrLn $ "Day 01:\n\tTask 1: " ++ show e1

    let ds' = map (filter isDigit . w2d "") ls
        f'  = map head ds'
        l'  = map last ds'
        e2  = sum $ zipWith (\a b -> read @Int [a,b]) f' l'
    putStrLn $ "\tTask 2: " ++ show e2

    pure ()
