{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE TypeApplications #-}

module Day09 (run) where

sub :: [Int] -> [Int]
sub (x1:x2:xs) = (x1 - x2) : sub (x2:xs)
sub _          = []

predict :: [Int] -> Int
predict []                                 = undefined
predict history@(h:_) | all (==0) history' = h
                      | otherwise          = h + predict history'
  where
     history' = sub history

sub' :: [Int] -> [Int]
sub' (x1:x2:xs) = (x2 - x1) : sub' (x2:xs)
sub' _          = []

predict' :: [Int] -> Int
predict' []                                 = undefined
predict' history@(h:_) | all (==0) history' = h
                       | otherwise          = h - predict' history'
  where
     history' = sub' history

run :: IO ()
run = do
    input <- map (map (read @Int) . words) . lines
            <$> readFile "./inputs/day09.txt"

    let predictions = map (predict . reverse) input
        e1          = sum predictions

    putStrLn $ "Day 09:\n\tTask 1: " ++ show e1

    let predictions' = map predict' input
        e2           = sum predictions'

    putStrLn $ "\tTask 2: " ++ show e2
