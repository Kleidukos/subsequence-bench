{-# LANGUAGE LambdaCase #-}

module Subsequence where

import Data.Bits (FiniteBits (finiteBitSize), countTrailingZeros, unsafeShiftR)
import Data.WideWord.Word128
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
      whole@(x : _) ->
        if bitsLeft == 0
          then Gen.word64 (Range.constant 0 10000) >>= go rest (finiteBitSize @Word64 undefined)
          else
            let !shift = min bitsLeft (countTrailingZeros encoding)
             in if shift == 0
                  then (x :) <$> go (drop 1 whole) (bitsLeft - 1) (encoding `unsafeShiftR` 1)
                  else go (drop shift whole) (bitsLeft - shift) (encoding `unsafeShiftR` shift)

subsequence128 :: (MonadGen m) => [a] -> m [a] 
subsequence128 list = shrink Shrink.list $ case list of
  [] -> pure []
  [x] -> Gen.element [[], [x]]
  src -> do
    first <- Gen.word64 (Range.constant 0 10000)
    second <- Gen.word64 (Range.constant 0 10000) 
    go src (finiteBitSize @Word128 undefined) (Word128 first second)
  where
    go :: (MonadGen m) => [a] -> Int -> Word128 -> m [a]
    go rest !bitsLeft !encoding = case rest of
      [] -> pure []
      whole@(x : _) ->
        if bitsLeft == 0
          then do
            first <- Gen.word64 (Range.constant 0 10000)
            second <- Gen.word64 (Range.constant 0 10000) 
            go rest (finiteBitSize @Word128 undefined) (Word128 first second)
          else
            let !shift = min bitsLeft (countTrailingZeros encoding)
             in if shift == 0
                  then (x :) <$> go (drop 1 whole) (bitsLeft - 1) (encoding `unsafeShiftR` 1)
                  else go (drop shift whole) (bitsLeft - shift) (encoding `unsafeShiftR` shift)
