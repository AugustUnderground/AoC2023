{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Day05 (run) where

import Data.List.Split (splitOn)

data Range = Range { srcStart :: Int
                   , srcStop  :: Int
                   , dstStart :: Int
                   , dstStop  :: Int
                   } deriving (Show, Eq, Ord)

toRange :: [Int] -> Range
toRange [d,s,l] = Range s (s + l - 1) d (d + l - 1)
toRange _       = undefined

mapping :: [String] -> [Range]
mapping = map (toRange . map (read @Int) . words)

inRange :: Int -> Range -> Bool
inRange src Range{..} = (src >= srcStart) && (src <= srcStop)

convert' :: Range -> Int -> Int
convert' Range{..} src = (src - srcStart) + dstStart

convert :: [Range] -> Int -> Int
convert []             src                     = src
convert (range:ranges) src | inRange src range = convert' range src 
                           | otherwise         = convert ranges src 

seed2loc :: [[Range]] -> Int -> Int
seed2loc r s = foldl (flip convert) s r 

seedRanges :: [Int] -> [(Int,Int)]
seedRanges (s:s':ss) = (s, s + s' - 1):seedRanges ss
seedRanges _         = []

inRange' :: Int -> Int -> Range -> Bool
inRange' start stop Range{..} = not (  (srcStart < start && srcStop < start)
                                    || (srcStart > stop  && srcStop > stop ) )

src2dst :: Range -> Int -> Int -> (Int,Int)
src2dst Range{..} start stop = (start',stop')
  where
    start' = dstStart + (start - srcStart)
    stop'  = dstStart + (stop  - srcStart)

convertRange :: [Range] -> (Int,Int) -> [(Int,Int)]
convertRange ranges (start, stop) = [ src2dst r (max start srcStart) (min stop srcStop)
                                    | r@Range{..} <- ranges, inRange' start stop r ]

convertRange' :: [Range] -> [(Int,Int)] -> [(Int,Int)]
convertRange' ranges = concatMap (convertRange ranges)

convertRanges :: [[Range]] -> (Int,Int) -> [(Int,Int)]
convertRanges rs src = foldl (flip convertRange') [src] rs

run :: IO ()
run = do
    input <- splitOn [""] . lines <$> readFile "./inputs/day05.txt"
    let seeds = map (read @Int) . words . dropWhile (/=' ') . head $ head input
        mrs   = map (mapping . tail) $ tail input
        locs  = map (seed2loc mrs) seeds
        e1    = minimum locs
        
    putStrLn $ "Day 05:\n\tTask 1: " ++ show e1

    let seeds' = seedRanges seeds
        e2 = minimum . map fst $ concatMap (convertRanges mrs) seeds'

    putStrLn $ "\tTask 2: " ++ show e2
