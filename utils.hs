module Utils where

import Data.Char
import Data.Ord

strToAlphabetValues :: String -> [Int]
strToAlphabetValues = map charToAlphabetPos

alphabetValuesToStr :: [Int] -> String
alphabetValuesToStr = map alphabetPosToChar

charToAlphabetPos :: Char -> Int
charToAlphabetPos c = (ord . toUpper $ c) - 65

alphabetPosToChar :: Int -> Char
alphabetPosToChar x = chr $ x + 65
