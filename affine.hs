module Affine where
import Utils
import Data.Maybe

encipherAffine :: String -> (Int, Int) -> String
encipherAffine _ (0, _) = "Invalid key, fst needs to be >= 0"
encipherAffine pt key 
    | gcd (fst key) 26 /= 1 = "Invalid key, fst needs to be coprime with 26"
    | otherwise = alphabetValuesToStr $ map ((`mod` 26) . ((+ (snd key)) . (* (fst key)))) $ strToAlphabetValues pt

decipherAffine :: String -> (Int, Int) -> String
decipherAffine ct key = alphabetValuesToStr $ map ((`mod` 26) . ((* (fromJust (getMultiplicativeInverse (fst key)))) . (+ (-1)* (snd key)))) $ strToAlphabetValues ct

