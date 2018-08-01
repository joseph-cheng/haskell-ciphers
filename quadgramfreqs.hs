module QuadgramFreqs where
import System.IO.Unsafe
import Data.Array

quadgramFreqs :: Array Int Float
quadgramFreqs = listArray (0, 456975) [x |x <- map read  $ lines  $ unsafePerformIO $ readFile $ "quadgram_freqs.txt"]
