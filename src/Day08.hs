{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE TypeApplications #-}

module Day08 (run) where

import           Data.List            (singleton)
import           Data.Map             (Map, (!))
import qualified Data.Map        as M
import qualified Data.Set        as S

data Dir = L | R
    deriving (Eq, Show, Read)

type Dirs = (String, String)

go :: Dir -> Dirs -> String
go L = fst
go R = snd

parseNodeList :: [String] -> Map String (String,String)
parseNodeList []     = M.empty
parseNodeList (l:ls) = M.insert nodeId (left',right') $ parseNodeList ls
  where
    nodeId = take 3 l
    left'  = take 3 $ drop 7 l
    right' = take 3 $ drop 12 l

numSteps :: Map String (String, String) -> [Dir] -> String -> Int
numSteps _    _      "ZZZ" = 0
numSteps map' (d:ds) node  = succ $ numSteps map' ds node'
  where
    node' = go d $ map' ! node
numSteps _    _      _     = undefined

numSteps' :: Map String (String, String) -> [Dir] -> String -> Int
numSteps' map' (d:ds) node | last node == 'Z' = 0
                           | otherwise = succ $ numSteps' map' ds node'
  where
    node' = go d $ map' ! node
numSteps' _    _      _     = undefined

run :: IO ()
run = do
    input <- lines <$> readFile "./inputs/day08.txt"

    let directions = cycle . map (read @Dir . singleton) $ head input
        nodes      = parseNodeList $ drop 2 input
        e1         = numSteps nodes directions "AAA"

    putStrLn $ "Day 08:\n\tTask 1: " ++ show e1

    let starts = S.toList . S.filter ((=='A') . last) $ M.keysSet nodes
        steps  = map (numSteps' nodes directions) starts
        e2     = foldl1 lcm steps

    putStrLn $ "\tTask 2: " ++ show e2
