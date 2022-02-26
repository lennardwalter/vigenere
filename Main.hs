module Main where

import Analysis (findKey)
import Vigenère (encipher)

main :: IO ()
main =
  do
    let key = "test"
    let plaintext = "Hallo, dies ist ein Test. Der Plaintext muss eine gewissen Laenge haben, damit die Haeufigkeitsanalyse funktioniert."
    let ciphertext = encipher key plaintext
    let recoveredKey = findKey ciphertext $ length key
    putStrLn $ "recovered key: " ++ recoveredKey
