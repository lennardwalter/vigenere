module Main where

import Analysis (findKeyInRange)
import Vigenère (encipher)

main :: IO ()
main =
  do
    let key = "test"
    let plaintext = "Hallo, dies ist ein Test. Der Plaintext muss eine gewissen Laenge haben, damit die Haeufigkeitsanalyse funktioniert."
    let ciphertext = encipher key plaintext
    let recoveredKey = findKeyInRange ciphertext (2, 5)
    putStrLn $ "recovered key: " ++ recoveredKey
