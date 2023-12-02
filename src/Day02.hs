{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}

module Day02 (run) where

import           Data.Map             (Map)
import qualified Data.Map        as M
import           Data.List.Split      (splitOn)

limit :: Map Cube Int
limit = M.fromList [(Red, 12), (Green, 13), (Blue, 14)]

data Cube = Red | Green | Blue
    deriving (Show, Ord, Eq)

instance Read Cube where
  readsPrec _ "red"   = [(Red, "")]
  readsPrec _ "green" = [(Green, "")]
  readsPrec _ "blue"  = [(Blue, "")]
  readsPrec _ _       = undefined

numCubes :: String -> (Cube,Int)
numCubes sample = (read @Cube c', read @Int n')
  where
    [n',c'] = words sample
    
getGames :: [String] -> Map Int [Map Cube Int]
getGames [] = M.empty
getGames ls = M.insert id' cubes . getGames $ tail ls
  where
    [gid, samples] = splitOn ":" $ head ls
    id'            = read @Int $ drop 5 gid
    samples'       = splitOn ";" samples
    cubes          = map (M.fromList . map numCubes . splitOn ",") samples'
    
run :: IO ()
run = do
    games <- getGames . lines <$> readFile "./inputs/day02.txt"

    let maxCubes = M.map (M.unionsWith max) games
        possible = M.map (and . M.elems . M.map (0<=) . M.unionWith (-) limit) maxCubes
        e1       = M.foldrWithKey (\id' p e -> if p then id' + e else e) 0 possible
        e2       = M.foldr (+) 0 $ M.map (M.foldr (*) 1) maxCubes 

    putStrLn $ "Day 02:\n\tTask 1: " ++ show e1
    putStrLn $ "\tTask 2: "          ++ show e2
