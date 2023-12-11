{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE TupleSections #-}

module Day11 (run) where

import Data.List (transpose,findIndices,elemIndices,intersect)

type Universe  = [[Char]]
type Coordinate = (Int, Int)

findEmpty :: Universe -> [Int]
findEmpty = findIndices (notElem '#')

expand' :: Universe -> [Int] -> Int -> Universe -> Universe
expand' universe indices idx universe' | idx `elem` indices = double
                                       | otherwise          = single
  where
    double = (universe !! idx):(universe !! idx):universe'
    single = (universe !! idx):universe'

expand :: Universe -> Universe
expand universe = universe'
  where
    nRows     = length universe
    nCols     = length $ head universe
    rows      = findEmpty universe
    cols      = findEmpty $ transpose universe
    horiz     = foldr (expand' universe rows) [] [0 .. pred nRows]
    universe' = transpose $ foldr (expand' (transpose horiz) cols) [] [0 .. pred nCols]

findGalaxies :: Universe -> [Coordinate]
findGalaxies universe = concat . zipWith (\r cs -> map (r,) cs) [0 ..]
                      $ map (elemIndices '#') universe

galaxyPairs :: [Coordinate] -> [(Coordinate, Coordinate)]
galaxyPairs [] = []
galaxyPairs (c:cs) = map (c,) cs ++ galaxyPairs cs

distance :: Coordinate -> Coordinate -> Int
distance (r1,c1) (r2,c2) = d
  where
    r = abs $ r1 - r2
    c = abs $ c1 - c2
    d = r + c

distances :: [(Coordinate,Coordinate)] -> [Int]
distances [] = []
distances ((c1,c2):coords) = d : distances coords
  where
    d = distance c1 c2

distance' :: [Int] -> [Int] -> Coordinate -> Coordinate -> Int
distance' emptyRows emptyCols (r1,c1) (r2,c2) = d
  where
    f   = 1000000
    r'  = length $ [ min r1 r2 .. max r1 r2 ] `intersect` emptyRows
    c'  = length $ [ min c1 c2 .. max c1 c2 ] `intersect` emptyCols
    r   = abs $ r1 - r2
    c   = abs $ c1 - c2
    r'' = (r - r') + (r' * f)
    c'' = (c - c') + (c' * f)
    d   = r'' + c''

distances' :: [Int] -> [Int] -> [(Coordinate,Coordinate)] -> [Int]
distances' _ _ [] = []
distances' r c ((c1,c2):coords) = d : distances' r c coords
  where
    d = distance' r c c1 c2

run :: IO ()
run = do
    universe' <- lines <$> readFile "./inputs/day11.txt"

    let universe = expand universe'
        galaxies = findGalaxies universe
        pairs    = galaxyPairs galaxies
        e1       = sum $ distances pairs

    putStrLn $ "Day 11:\n\tTask 1: " ++ show e1
    -- putStr $ unlines universe

    let galaxies' = findGalaxies universe'
        pairs'    = galaxyPairs galaxies'
        emptyR    = findEmpty universe'
        emptyC    = findEmpty $ transpose universe'
        e2        = sum $ distances' emptyR emptyC pairs'

    putStrLn $ "\tTask 2: " ++ show e2
