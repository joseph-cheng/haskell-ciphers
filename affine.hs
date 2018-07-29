module Affine where
import Utils

encipherAffine :: String -> (Int, Int) -> String
encipherAffine _ (0, _) = "Invalid key, fst needs to be >= 0"
encipherAffine pt key = alphabetValuesToStr $ map ((`rem` 26) . ((+ (snd key)) . (* (fst key)))) $ strToAlphabetValues pt

decipherAffine :: String -> (Int, Int) -> String
decipherAffine ct key = encipherAffine ct (key)
