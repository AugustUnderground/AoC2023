{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Day03 (run) where

import           Data.Char       (isDigit)
import           Data.List       (elemIndices, isPrefixOf)
import           Data.List.Split (splitWhen)
import           Data.Set        (Set)
import qualified Data.Set as S

type PartNumber = Int

isSymbol :: Char -> Bool
isSymbol c = not $ isDigit c || (c == '.')

coordinates :: Int -> [[Bool]] -> Set (Int, Int)
coordinates _ [] = S.empty
coordinates rowId (row:rows) = S.union coords $ coordinates (succ rowId) rows
  where
    coords = S.fromList . map (rowId, ) $ elemIndices True row

surroundings :: Int -> Int -> (Int,Int) -> Set (Int,Int)
surroundings nr nc (r,c) = S.fromList 
                         $ filter (\(r',c') -> (r' >= 0) && (r' < nr) && (c' >= 0) && (c' < nc)) surr
  where
    surr = [ (r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)
           , (r - 1, c - 1), (r - 1, c + 1), (r + 1, c - 1), (r + 1, c + 1) ]

partNumbers' :: String -> [PartNumber]
partNumbers' = map (read @PartNumber) 
             . filter (not . null) 
             . map (filter isDigit) 
             . filter (not . null) 
             . splitWhen (not . isDigit)

partNumberSurroundings :: PartNumber -> Int -> Int -> Int -> Int -> Set (Int,Int)
partNumberSurroundings partNumber rowId colId numRows numCols = surr
  where
    len      = pred . length $ show partNumber
    colRange = [colId .. colId + len]
    surr     = S.unions . map (surroundings numRows numCols . (rowId,) ) $ colRange

surroundings' :: Int -> Int -> Int -> Int -> String -> [PartNumber] -> [(PartNumber, Set (Int,Int))]
surroundings' _ _ _ _ "" _ = []
surroundings' _ _ _ _ _ [] = []
surroundings' numRows numCols rowId colId row p@(pn:pns) 
    | pn' `isPrefixOf` row = (pn, sur) : surroundings' numRows numCols rowId colId' row' pns
    | otherwise            = surroundings' numRows numCols rowId (succ colId) (tail row) p
  where
    pn'    = show pn
    sur    = partNumberSurroundings pn rowId colId numRows numCols
    row'   = drop (length pn') row
    colId' = colId + length pn'

getPartNumbers :: Int -> Int -> Int -> [String] -> [(PartNumber, Set (Int,Int))]
getPartNumbers _ _ _     []     = []
getPartNumbers numRows numCols rowId (r:rs) = surr ++ getPartNumbers numRows numCols (succ rowId) rs
  where
    pns = partNumbers' r
    surr = surroundings' numRows numCols rowId 0 r pns

gearRatio :: [(PartNumber, Set (Int, Int))] -> (Int, Int) -> Int
gearRatio parts starCoord | length pns == 2 = product pns
                          | otherwise       = 0
  where
    pns = map fst $ filter (S.member starCoord . snd) parts

run :: IO ()
run = do
    schematic <- lines <$> readFile "./inputs/day03.txt"

    let symbols      = map (map isSymbol) schematic
        symbolCoords = coordinates 0 symbols
        numRows      = length schematic :: Int
        numCols      = length $ head schematic :: Int
        partNumbers  = getPartNumbers numRows numCols 0 schematic
        e1           = sum . map fst $ filter (not . S.disjoint symbolCoords . snd) partNumbers

    putStrLn $ "Day 03:\n\tTask 1: " ++ show e1

    let starCoords = coordinates 0 $ map (map (=='*')) schematic
        e2 = sum . map (gearRatio partNumbers) $ S.toList starCoords

    putStrLn $ "\tTask 2: " ++ show e2
