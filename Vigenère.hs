module Vigenère where

import Data.Char (chr, ord)
import Data.Maybe (mapMaybe)

-- | Encipher a string
encipher :: String -- ^ Key
         -> String -- ^ Plaintext
         -> String -- ^ Ciphertext
encipher = cipher encipherPair

-- | Encipher a pair of characters
encipherPair :: (Char, Char) -> Char
encipherPair (a, b) = chr $ (ord a + ord b) `mod` 26 + 65

-- | Decipher a string
decipher :: String -- ^ Key
         -> String -- ^ Ciphertext
         -> String -- ^ Plaintext
decipher = cipher decipherPair

-- | Decipher a pair of characters
decipherPair :: (Char, Char) -> Char
decipherPair (a, b) = chr $ (ord a - ord b) `mod` 26 + 65

-- | Generic cipher operation
cipher :: ((Char, Char) -> Char) -> String -> String -> String
cipher f key text = map f $ zip text' $ cycle key'
  where
    key' = sanitize key
    text' = sanitize text

-- | Sanitize a string by removing all non-alphabetic characters and converting to uppercase
sanitize :: String -> String
sanitize = mapMaybe sanitizeChar
  where
    sanitizeChar c
      | c `elem` ['A' .. 'Z'] = Just c
      | c `elem` ['a' .. 'z'] = Just $ chr $ ord c - 32
      | otherwise = Nothing
