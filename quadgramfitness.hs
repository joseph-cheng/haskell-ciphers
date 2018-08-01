module QuadgramFitness where
import QuadgramFreqs
import Utils


getFitness :: String -> Float
getFitness st = sum [quadgramFreqs !! (17576 * (charToAlphabetPos (text !! i)) + 676 * (charToAlphabetPos (text !! (i+1))) + 26 * (charToAlphabetPos (text !! (i+2))) + (charToAlphabetPos (text !! (i+3))))| i <- [0..((length text) - 4)]]
    where text = sanitiseString st


