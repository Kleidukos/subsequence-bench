import Test.Tasty.Bench

import Hedgehog.Gen qualified as Gen
import qualified Subsequence
import qualified SublistOf
import Test.Tasty.Patterns.Printer
import Test.QuickCheck qualified as QC

main :: IO ()
main = defaultMain
  [ bgroup "Subsequence" $ sublistOfBenches [10, 20..100]
  ]

sublistOfBenches :: [Int] -> [Benchmark]
sublistOfBenches sizes =
  sizes >>= \size ->
    let baselineName = "Hedgehog subsequence (native, " <> show size <> " items"
     in [ bench baselineName
            . nfIO
            . Gen.sample
            . Gen.subsequence
            . make42s
            $ size
        , bcompare (findBench baselineName)
            . bench ("Hedgehog subsequence64, optimised, " <> show size <> " items")
            . nfIO
            . Gen.sample
            . Subsequence.subsequence64
            . make42s
            $ size
        , bcompare (findBench baselineName)
            . bench ("QC sublistOf, native, " <> show size <> " items")
            . nfIO
            . QC.generate
            . QC.sublistOf
            . make42s
            $ size
        , bcompare (findBench baselineName)
            . bench ("QC sublistOf64, optimised, " <> show size <> " items")
            . nfIO
            . QC.generate
            . SublistOf.sublistOf64
            . make42s
            $ size
        ]

findBench :: String -> String
findBench = printAwkExpr . locateBenchmark . (: [])

make42s :: Int -> [Integer]
make42s count = replicate count 42
