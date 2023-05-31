{-# OPTIONS_GHC -O2 #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
module SublistOf where

import Test.QuickCheck
import Data.Kind
import Data.Word (Word64)
import Data.Bits hiding (shift)

sublistOf64 ::
  forall (a :: Type).
  [a] ->
  Gen [a]
sublistOf64 = \case
  [] -> pure []
  [x] -> elements [[], [x]]
  src -> arbitrary >>= go src (finiteBitSize @Word64 undefined)
  where
    go :: [a] -> Int -> Word64 -> Gen [a]
    go rest !bitsLeft !encoding = case rest of
      [] -> pure []
      (x : xs) ->
        let !shift = min bitsLeft (countTrailingZeros encoding) in
          if | shift > 0 -> go (drop shift rest) (bitsLeft - shift) (encoding `unsafeShiftR` shift)
             | bitsLeft == 0 -> arbitrary >>= go rest (finiteBitSize @Word64 undefined)
             | otherwise -> (x :) <$> go xs (bitsLeft - 1) (encoding `unsafeShiftR` 1)
