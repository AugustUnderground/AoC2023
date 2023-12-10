{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE TypeApplications #-}

module Day10 (run) where

import           Data.List     (elemIndex)
import           Data.Maybe    (isJust)
import           Data.Set      (Set)
import qualified Data.Set as S

type Grid  = [[Char]]
type Coord = (Int,Int)
type Pipe  = Char

findStart :: Grid -> Coord
findStart g = (sx,sy)
  where
    (Just sx,sy) = head . filter (isJust . fst) . flip zip [0 .. ]
                 $ map (elemIndex 'S') g

inBounds :: Grid -> Coord -> Bool
inBounds g (x,y) = (x >= 0) && (y >= 0) && (x < x') && (y < y')
  where
    x' = length $ head g
    y' = length g

isConnected :: Coord -> Pipe -> Coord -> Bool
isConnected (x,y) '|' (x',y') = (x' == x) && ((y' == succ y) || (y' == pred y))
isConnected (x,y) '-' (x',y') = (y' == y) && ((x' == succ x) || (x' == pred x))
isConnected (x,y) 'L' (x',y') = ((y' == y) && (x' == succ x))
                              || ((x' == x) && (y' == pred y))
isConnected (x,y) 'J' (x',y') = ((y' == y) && (x' == pred x))
                              || ((x' == x) && (y' == pred y))
isConnected (x,y) '7' (x',y') = ((y' == y) && (x' == pred x))
                              || ((x' == x) && (y' == succ y))
isConnected (x,y) 'F' (x',y') = ((y' == y) && (x' == succ x))
                              || ((x' == x) && (y' == succ y))
isConnected _      _  _       = False

pipe :: Grid -> Coord -> Pipe
pipe g (x,y) = (g !! y) !! x

nextCoords' :: Coord -> Pipe -> Set Coord
nextCoords' (x,y) '|' = S.fromList [(x,y+1), (x,y-1)]
nextCoords' (x,y) '-' = S.fromList [(x+1,y), (x-1,y)]
nextCoords' (x,y) 'L' = S.fromList [(x,y-1), (x+1,y)]
nextCoords' (x,y) 'J' = S.fromList [(x,y-1), (x-1,y)]
nextCoords' (x,y) '7' = S.fromList [(x,y+1), (x-1,y)]
nextCoords' (x,y) 'F' = S.fromList [(x,y+1), (x+1,y)]
nextCoords' _      _  = undefined

nextCoords :: Grid -> Coord -> Set Coord
nextCoords g c@(x,y) | pipe g c == 'S' = coords
                     | inBounds g c    = nextCoords' c $ pipe g c
                     | otherwise       = S.empty
  where
    coords = S.fromList [ c' | c' <- [(x-1,y),(x+1,y),(x,y-1),(x,y+1)]
                        , inBounds g c', isConnected c' (pipe g c') (x,y) ]

trav :: Grid -> Set Coord -> Set Coord -> Int -> [Int]
trav grid coords visited step | S.null coords = []
                              | otherwise     = step' : trav grid coords' visited' step'
  where
    coords'' = S.foldr (\c c' -> S.union c' $ nextCoords grid c) S.empty coords
    coords'  = S.difference coords'' visited
    step'    = succ step
    visited' = S.union coords visited

replaceStart :: Grid -> Coord -> Pipe
replaceStart g (x,y) | (x1 == pred x) && (x2 == x) && (y1 == y) && (y2 == succ y) = '7'
                     | (x1 == x) && (x2 == succ x) && (y1 == y) && (y2 == succ y) = 'F'
                     | (x1 == pred x) && (x2 == x) && (y1 == y) && (y2 == pred y) = 'J'
                     | (x1 == x) && (x2 == succ x) && (y1 == y) && (y2 == pred y) = 'L'
                     | (x1 == x) && (x2 == x) = '|'
                     | (y1 == y) && (y2 == y) = '-'
                     | otherwise = 'S'
  where
    [(x1,y1),(x2,y2)] = [ c' | c' <- [(x-1,y),(x+1,y),(x,y+1),(x,y-1)]
                        , inBounds g c'
                        , isConnected c' (pipe g c') (x,y) ]

trav' :: Grid -> Coord -> Set Coord -> Set Coord
trav' grid coord visited | S.null coord'' = visited'
                         | otherwise     = trav' grid coord' visited'
  where
    coord''  = S.difference (nextCoords grid coord) visited
    coord'   = S.elemAt 0 coord''
    pipe'    = if pipe grid coord' == 'S'
                  then replaceStart grid coord'
                  else pipe grid coord'
    visited' = S.insert coord visited

gridCoords :: Grid -> [[Coord]]
gridCoords grid = [ [(x,y) | x <- [ 0 .. x' ]] | y <- [ 0 .. y' ]]
  where
    x' = pred . length $ head grid
    y' = pred . length $ grid

isInside :: Grid -> Set Coord -> Bool -> Int -> [Coord] -> Int
isInside _    _      _    counter  []    = counter
isInside grid path inside counter (c:cs) 
    | S.member c path = isInside grid path inside' counter cs 
    | inside          = isInside grid path inside (succ counter) cs 
    | otherwise       = isInside grid path inside counter cs 
  where
    pipes = S.fromList "|LJ"
    p     = pipe grid c
    inside' = if S.member p pipes then not inside else inside

run :: IO ()
run = do
    input <- lines <$> readFile "./inputs/day10.txt"

    let start' = findStart input
        start  = S.singleton start'
        e1     = pred . maximum $ trav input start S.empty 0

    putStrLn $ "Day 10:\n\tTask 1: " ++ show e1

    let path = trav' input start' S.empty
        grid = gridCoords input
        e2   = sum $ map (isInside input path False 0) grid

    putStrLn $ "\tTask 2: " ++ show e2
    pure ()
