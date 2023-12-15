{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Day15 (run) where

import           Data.List.Split (splitOn)
import           Data.Char       (ord)
import           Data.List       (findIndex)
import           Data.Map        (Map, (!))
import qualified Data.Map as M

data Operation = Set String Int
               | Remove String
               deriving (Show)

type Lens  = (String, Int)
type Boxes = Map Int [Lens]

hash :: Int -> String -> Int
hash v ""    = v
hash v (c:s) = hash v' s
  where
    v' =  (`rem` 256) . (*17) . (+v) $ ord c

operations :: [String] -> [Operation]
operations [] = []
operations (s:ss) | '-' `elem` s = ro : operations ss
                  | otherwise    = rs : operations ss
  where
    ro = Remove $ takeWhile (/='-') s
    [s',l'] = splitOn "=" s
    rs = Set s' $ read @Int l'

update :: [Lens] -> Lens -> Int -> [Lens]
update ls l idx = take idx ls ++ [l] ++ drop (succ idx) ls

doOp :: Boxes -> Operation -> Boxes
doOp boxes (Remove l) = boxes'
  where
    boxId  = hash 0 l
    boxes' = M.adjust (filter ((/=l) . fst)) boxId boxes
doOp boxes (Set l i) = boxes'
  where
    boxId   = hash 0 l
    lenses  = boxes ! boxId
    lens    = (l,i)
    lenses' = maybe (lenses ++ [lens]) (update lenses lens)
            $ findIndex ((==l) . fst) lenses
    boxes'  = M.adjust (const lenses') boxId boxes

focusingPower :: Int -> Int -> [Lens] -> Int
focusingPower fp boxId lenses = fp + fp'
  where
    fp' = sum . map (* succ boxId) . zipWith (*) [ 1 .. ] $ map snd lenses

run :: IO ()
run = do
    input <- splitOn "," . filter (/='\n') <$> readFile "./inputs/day15.txt"
    let e1 = sum $ map (hash 0) input

    putStrLn $ "Day 15:\n\tTask 1: " ++ show e1

    let ops   = operations input
        boxes = M.fromList $ map (,[]) [ 0 .. 255 ]
        e2 = M.foldlWithKey focusingPower 0 $ foldl doOp boxes ops

    putStrLn $ "\tTask 2: " ++ show e2
