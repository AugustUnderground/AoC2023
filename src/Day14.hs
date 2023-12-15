{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE FlexibleContexts #-}

module Day14 (run) where

import           Data.Maybe     (fromJust)
import           Data.List      (elemIndex)
import           Data.Map       (Map)
import           Data.Set       (Set)
import qualified Data.Map as M
import qualified Data.Set as S

type Rock       = Char
type Coordinate = (Int,Int)

subtract' :: Coordinate -> Coordinate -> Coordinate
subtract' (a1,b1) (a2,b2) = (a2 - a1, b2 - b1)

tilt :: Map Coordinate Rock -> Set Coordinate -> Set Coordinate -> Coordinate
     -> Set Coordinate
tilt b c r d | r == r'   = r
             | otherwise = tilt b c r' d
  where
    free = S.difference (M.keysSet b) $ S.union r c
    r'   = S.map (\z -> let z' = subtract' d z
                         in if S.member z' free then z' else z) r

load :: Int -> Set Coordinate -> Int
load l = S.foldr (\r' c' -> (c' +) . (l -) $ fst r') 0

spin :: Map Coordinate Rock -> Set Coordinate -> Set Coordinate -> Set Coordinate
spin b c r = foldl (tilt b c) r [(1,0), (0,1), (-1,0), (0,-1)]

spinCycle :: Map Coordinate Rock -> Set Coordinate -> Set Coordinate -> Int
          -> [Set Coordinate] -> Set Coordinate
spinCycle b c r i s | r' `elem` s = reverse s !! idx
                    | otherwise   = spinCycle b c r' i' s'
  where
    i'    = succ i
    r'    = spin b c r
    s'    = r' : s
    start = fromJust . elemIndex r' $ reverse s
    idx   = (1000000000 - i) `mod` (start - i) + i - 1

run :: IO ()
run = do
    input <- lines <$> readFile "./inputs/day14.txt"
    let board  = M.fromList . concat $ zipWith (\i l -> zipWith (\j x -> ((i,j), x)) [0 ..] l) [0 ..] input
        rounds = M.keysSet $ M.filter (=='O') board
        cubes  = M.keysSet $ M.filter (=='#') board
        len    = length input
        e1     = load len $ tilt board cubes rounds (1,0) 

    putStrLn $ "Day 14:\n\tTask 1: " ++ show e1

    let e2 = load len $ spinCycle board cubes rounds 0 []

    putStrLn $ "\tTask 2: " ++ show e2
