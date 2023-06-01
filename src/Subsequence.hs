{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE MultiWayIf #-}
module Subsequence where

import Data.Bits (FiniteBits (finiteBitSize), countTrailingZeros, unsafeShiftR)
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
  src -> Gen.word64 (Range.constant minBound maxBound) >>= go src (finiteBitSize @Word64 undefined)
  where
    go :: (MonadGen m) => [a] -> Int -> Word64 -> m [a]
    go rest !bitsLeft !encoding = case rest of
      [] -> pure []
      (x : xs) ->
        let !shift = min bitsLeft (countTrailingZeros encoding) in
          if | shift > 0 -> go (drop shift rest) (bitsLeft - shift) (encoding `unsafeShiftR` shift)
             | bitsLeft == 0 -> 
                 Gen.word64 (Range.constant minBound maxBound)
                    >>= go rest (finiteBitSize @Word64 undefined)
             | otherwise -> (x :) <$> go xs (bitsLeft - 1) (encoding `unsafeShiftR` 1)
