module Caesar where
import Utils

encipherCaesar :: String -> Int -> String
encipherCaesar pt key = alphabetValuesToStr $ map ((`rem` 26) . (+key)) $ strToAlphabetValues pt

decipherCaesar :: String -> Int -> String
decipherCaesar ct key = encipherCaesar ct (-key)




