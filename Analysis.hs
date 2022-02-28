module Analysis where

import Control.Monad (guard)
import Data.Char (chr)
import Data.List (genericLength, minimumBy, transpose)
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import Helpers (elemCount, minIndex, splitEvery, stringSearch)
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

-- | Calculate the performance in terms of matching the frequency distribution of German monograms
monogramPerformance :: String -> Float
monogramPerformance s = chiSquared (monogramFrequencies s) germanMonogramFrequencies

-- | Find the most likely key used to encipher the given ciphertext, providing the key length
findKeyWithLength :: String -> Int -> String
findKeyWithLength ciphertext keylength =
  map bestColumnKey columns
  where
    ciphertext' :: String
    ciphertext' = sanitize ciphertext

    columns :: [[Char]]
    columns = transpose $ splitEvery keylength ciphertext'

    bestColumnKey :: [Char] -> Char
    bestColumnKey column = chr $ (minIndex $ monogramPerformances) + 65
      where
        monogramPerformances = [monogramPerformance $ decipher [key] column | key <- ['A' .. 'Z']]

-- | Find the most likely key used to encipher the given ciphertext, providing a range of key lengths
findKeyInRange :: String -> (Int, Int) -> String
findKeyInRange ciphertext (from, to) =
  snd $ minimumBy (comparing fst) performances
  where
    performances :: [(Float, String)]
    performances =
      map
        ( \n ->
            let k = findKeyWithLength ciphertext n
             in (monogramPerformance $ decipher k ciphertext, k)
        )
        [from .. to]

-- | Perform key elimination on the given ciphertext to reveal the key
findKeyWithWordAndLength :: String -> String -> Int -> Maybe String
findKeyWithWordAndLength ciphertext word keylength = do
  posInCipher <- stringSearch (shiftedDiff word') (shiftedDiff ciphertext')
  guard (length key == keylength)
  pure key
  where
    ciphertext' :: String
    ciphertext' = sanitize ciphertext

    word' :: String
    word' = sanitize word

    key :: String
    key = take keylength $ decipher wordPart cipherPart

    posInCipher :: Maybe Int
    posInCipher = stringSearch (shiftedDiff word') (shiftedDiff ciphertext')

    cipherPart :: String
    cipherPart = take (length word' - keylength) (drop (fromJust posInCipher) ciphertext')

    wordPart :: String
    wordPart = take (length cipherPart) word'

    shiftedDiff :: String -> String
    shiftedDiff s = decipher (drop keylength s) (take (length s - keylength) s)
