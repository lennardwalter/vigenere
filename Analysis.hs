module Analysis where

import Data.Char (chr)
import Data.List (genericLength, transpose)
import Helpers (elemCount, minIndex, splitEvery)
import Vigenère (decipher, sanitize)

-- | German monogram frequencies in alphabetical order
germanMonogramFrequencies :: [Float]
germanMonogramFrequencies =
  [0.0634, 0.0221, 0.0271, 0.0492, 0.1599, 0.0180, 0.0302, 0.0411, 0.0760, 0.0027, 0.0150, 0.0372, 0.0275, 0.0959, 0.0275, 0.0106, 0.0004, 0.0771, 0.0641, 0.0643, 0.0376, 0.0094, 0.0140, 0.0007, 0.0013, 0.0122]

-- | Calculate the chi-squared statistic for the given distributions
chiSquared :: [Float] -> [Float] -> Float
chiSquared xs ys = sum $ [((x - y) ^ 2) / y | (x, y) <- zip xs ys]

-- | Calculate the relative monogram frequencies for the given text
monogramFrequencies :: String -> [Float]
monogramFrequencies s = [elemCount c s / genericLength s | c <- ['A' .. 'Z']]

-- | Calculate the performances in terms of matching the frequency distribution
--   of German monograms for deciphered text using each alphabetic character as a key
monogramPerformances :: String -> [Float]
monogramPerformances s = map performance ['A' .. 'Z']
  where
    performance c = chiSquared (monogramFrequencies (decipher [c] s)) germanMonogramFrequencies

-- | Find the most likely key used to encipher the given ciphertext
findKey :: String -> Int -> String
findKey ciphertext keylength =
  key
  where
    ciphertext' :: String
    ciphertext' = sanitize ciphertext

    columns :: [[Char]]
    columns = transpose $ splitEvery keylength ciphertext'

    key :: String
    key = map bestColumnShift columns

    bestColumnShift :: [Char] -> Char
    bestColumnShift column = chr $ (minIndex $ monogramPerformances column) + 65
