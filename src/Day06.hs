{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Day06 (run) where

travelDistances :: Int -> [Int]
travelDistances t = zipWith (*) [0 .. pred t] [ t, pred t .. ]

waysToWin :: Int -> [Int] -> Int
waysToWin r td = length $ filter (r<) td

waysToWin' :: Int -> Int -> Int
waysToWin' t d = x1 - x0
  where
    x3 = realToFrac t :: Double
    x2 = x3 / 2
    y2 = x2 ** 2
    a  = y2 / ((x2 ** 2) - (x2 * x3))
    b  = -a * x3
    c  = realToFrac d :: Double
    x' = sqrt $ b * b - 4 * a * (-c)
    x0 = ceiling $ (-b + x') / (2 * a)
    x1 = ceiling $ (-b - x') / (2 * a)

run :: IO ()
run = do
    input <- map (words . dropWhile (/=' ')) . lines
            <$> readFile "./inputs/day06.txt"

    let [times, distances] = map (map (read @Int)) $ input
        e1                 = product . zipWith waysToWin distances
                           $ map travelDistances times

    putStrLn $ "Day 06:\n\tTask 1: " ++ show e1

    let [time,distance] = map (read @Int . foldl1 (++)) input
        e2 = waysToWin' time distance

    putStrLn $ "\tTask 2: " ++ show e2
