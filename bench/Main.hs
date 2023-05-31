import Test.Tasty.Bench

import Hedgehog.Gen qualified as Gen
import qualified Subsequence
import qualified SublistOf
import Test.QuickCheck as QC

range :: [Int]
range = [1..10000]

main :: IO ()
main = defaultMain
  [ bgroup "Unoptimised subsequences"
      [ bench "Hedgehog subsequence" $ nfIO $ Gen.sample $ Gen.subsequence range
      , bench "QuickCheck sublistOf" $ nfIO $ QC.generate $ QC.sublistOf range
      ]
  , bgroup "64-bit field optimisation" 
     [ bench "Hedgehog subsequence with a bitfield of 64 elements" $ nfIO $ Gen.sample $ Subsequence.subsequence64 range
      , bench "QuickCheck sublistOf with a bitfield of 64 elements" $ nfIO $ QC.generate $ SublistOf.sublistOf64 range
    ]
  , bgroup "128-bit field optimisation"
    [ bench "Hedgehog subsequence with a bitfield of 64 elements" $ nfIO $ Gen.sample $ Subsequence.subsequence64 range
      , bench "QuickCheck sublistOf with a bitfield of 128 elements" $ nfIO $ QC.generate $ SublistOf.sublistOf128 range
    ]
  ]
