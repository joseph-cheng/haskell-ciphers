module Utils where

import Data.Char
import Data.Ord
import Data.List
import Data.Function

strToAlphabetValues :: String -> [Int]
strToAlphabetValues st = map charToAlphabetPos $ sanitiseString st

alphabetValuesToStr :: [Int] -> String
alphabetValuesToStr = map alphabetPosToChar

charToAlphabetPos :: Char -> Int
charToAlphabetPos c = (ord . toUpper $ c) - 65

alphabetPosToChar :: Int -> Char
alphabetPosToChar x = chr $ x + 65

getMultiplicativeInverse :: Int -> Maybe Int
getMultiplicativeInverse x 
    | gcd x 26 /= 1 = Nothing
    | otherwise = Just $ head $ dropWhile (\y -> y*x `rem` 26 /= 1) [1..26]

sanitiseString :: String -> String
sanitiseString st = map toUpper $ filter (isAlpha) st

analyseFrequency :: String -> [(Char, Float)]
analyseFrequency st = [(char, freq) | char <- ['A'..'Z'],
                                     let freq = (fromIntegral $ countOccurrences char $ sanitiseString st) / (fromIntegral $ length $ sanitiseString st)]

countOccurrences :: Eq a => a -> [a] -> Int
countOccurrences x st = length $ filter (==x) st

indexOfCoincidence :: String -> Float
indexOfCoincidence st = sum [(fromIntegral (n * (n-1))) / (fromIntegral (textLength * (textLength-1)))  | char <- ['A'..'Z'],
                                                                                                          let n = countOccurrences char $ sanitiseString st]
                            where textLength = length $ sanitiseString st
                                                        


