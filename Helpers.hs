module Helpers where

import Data.List
  ( findIndex,
    genericLength,
    isPrefixOf,
    minimumBy,
    tails,
  )
import Data.Ord (comparing)

-- | Split a given list into chunks length `0 < length <= n` (the last chunk may be shorter).
splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = first : (splitEvery n rest)
  where
    (first, rest) = splitAt n xs

-- | Returns the count of element 'x' in a given list
elemCount :: (Eq a, Num b) => a -> [a] -> b
elemCount x = genericLength . filter (== x)

-- | Find the index of the smallest element of a list.
minIndex :: (Ord a, Num b, Enum b) => [a] -> b
minIndex xs = snd $ minimumBy (comparing fst) (zip xs [0 ..])

-- | Find the start index of substring in a string or Nothing
stringSearch :: String -> String -> Maybe Int
stringSearch needle haystack = findIndex (isPrefixOf needle) (tails haystack)
