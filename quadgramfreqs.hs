module QuadgramFreqs where
import System.IO.Unsafe

quadgramFreqs :: [Float]
quadgramFreqs = map read  $ lines  $ unsafePerformIO $ readFile $ "quadgram_freqs.txt"
