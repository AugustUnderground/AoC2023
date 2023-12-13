{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Day12 (run) where

import           Data.List.Split      (splitOn)
import           Data.List            (group,elemIndices,intercalate)
import           Data.Tuple           (swap)
import           Control.Lens         (element, set)
import           Data.Map             ((!))
import qualified Data.Map        as M

process :: [String] -> (String,[Int])
process line = (str,con)
  where
    str = head line
    con = map (read @Int) . splitOn "," $ last line

contiguousGroups :: String -> [Int]
contiguousGroups =  map length . filter ((=='#') . head) . group

replace :: [Int] -> [Char] -> String -> String
replace (i:is) (c:cs) str = replace is cs $ set (element i) c str
replace []     _      str = str
replace _      []     str = str

arrange :: String -> [String]
arrange line = map (flip (replace idx) line) can
  where
    idx = elemIndices '?' line
    can = mapM (const ".#") [ 1 .. length idx ]

possible :: [Int] -> String -> [String]
possible groups = filter ((==groups) . contiguousGroups) . arrange

unfold' :: Int -> String -> [Int] -> (String, [Int])
unfold' n str con = (str', con')
  where
    str' = intercalate "?" $ replicate n str
    con' = take (n * length con) $ cycle con

memoize :: [Char] -> [Int] -> Int
memoize str groups = valids str groups
  where
    lenStr    = length str
    numGroups = length groups

    vals      = M.fromList [ ((s,i), valids' (drop s str) (drop i groups))
                           | s <- [0..lenStr], i <- [0.. numGroups]]

    valids :: [Char] -> [Int] -> Int
    valids s i = vals ! (lenStr - length s, numGroups - length i)

    valids' :: [Char] -> [Int] -> Int
    valids' [] []                     = 1
    valids' [] _                      = 0
    valids' ('#':_) []                = 0
    valids' ('.':str') is             = valids str' is
    valids' str'@('#':_) (i:is) | (length str'  >= i) &&
                                  ('.' `notElem` take i str') &&
                                  ((length str' == i) || (str' !! i /= '#'))
                                      = valids (drop (succ i) str') is
                                | otherwise     
                                      = 0
    valids' ('?':str') is             = valids str' is + valids' ('#':str') is
    valids' _ _                       = undefined

run :: IO ()
run = do
    input <- map (process . words) . lines <$> readFile "./inputs/day12.txt"
    let e1  = sum $ map (length . uncurry possible . swap) input

    putStrLn $ "Day 12:\n\tTask 1: " ++ show e1
    
    let e2 = sum $ map (uncurry memoize . uncurry (unfold' 5)) input
    
    putStrLn $ "\tTask 2: " ++ show e2
