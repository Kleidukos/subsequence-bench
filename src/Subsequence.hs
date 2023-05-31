module Subsequence where

import Data.Bits (FiniteBits (finiteBitSize))
import Data.Word (Word64)
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import qualified Hedgehog.Internal.Shrink as Shrink
import Hedgehog.Gen (shrink)

subsequence64 :: (MonadGen m) => [a] -> m [a] 
subsequence64 list = shrink Shrink.list $ case list of
  [] -> pure []
  [x] -> Gen.element [[], [x]]
  src -> Gen.word64 (Range.constant 0 10000) >>= go src (finiteBitSize @Word64 undefined)
  where
    go :: (MonadGen m) => [a] -> Int -> Word64 -> m [a]
    go rest !bitsLeft !encoding = case rest of
      [] -> pure []
      (x : xs) ->
        if bitsLeft == 0
          then Gen.word64 (Range.constant 0 10000) >>= go rest (finiteBitSize @Word64 undefined)
          else
            case encoding `quotRem` 2 of
              (encoding', 0) -> go xs (bitsLeft - 1) encoding'
              (encoding', _) -> (x :) <$> go xs (bitsLeft - 1) encoding'
