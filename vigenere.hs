module Vigenere where
import Utils

encipherVigenere :: String -> String -> String
encipherVigenere pt key = alphabetValuesToStr [((a+b) `mod` 26)|(a,b) <- zip (strToAlphabetValues pt) (cycle (strToAlphabetValues key))]

decipherVigenere :: String -> String -> String
decipherVigenere pt key = alphabetValuesToStr [((a-b) `mod` 26)|(a,b) <- zip (strToAlphabetValues pt) (cycle (strToAlphabetValues key))]
