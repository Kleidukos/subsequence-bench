{-# LANGUAGE LambdaCase #-}
module SublistOf where

import Test.QuickCheck
import Data.Kind
import Data.Word (Word64)
import Data.Bits hiding (shift)
import Data.WideWord
import GHC.Base (RuntimeRep(Word64Rep))

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
      whole@(x : _) ->
        if bitsLeft == 0
          then arbitrary >>= go rest (finiteBitSize @Word64 undefined)
          else
            let !shift = min bitsLeft (countTrailingZeros encoding)
             in if shift == 0
                  then (x :) <$> go (drop 1 whole) (bitsLeft - 1) (encoding `unsafeShiftR` 1)
                  else go (drop shift whole) (bitsLeft - shift) (encoding `unsafeShiftR` shift)

sublistOf128 ::
  forall (a :: Type).
  [a] ->
  Gen [a]
sublistOf128 = \case
  [] -> pure []
  [x] -> elements [[], [x]]
  src -> do
    first <- arbitrary @Word64 
    second <- arbitrary @Word64 
    go src (finiteBitSize @Word128 undefined) (Word128 first second)
  where
    go :: [a] -> Int -> Word128 -> Gen [a]
    go rest !bitsLeft !encoding = case rest of
      [] -> pure []
      whole@(x : _) ->
        if bitsLeft == 0
        then do
          first <- arbitrary @Word64 
          second <- arbitrary @Word64 
          go rest (finiteBitSize @Word128 undefined) (Word128 first second)
          else
            let !shift = min bitsLeft (countTrailingZeros encoding)
             in if shift == 0
                  then (x :) <$> go (drop 1 whole) (bitsLeft - 1) (encoding `unsafeShiftR` 1)
                  else go (drop shift whole) (bitsLeft - shift) (encoding `unsafeShiftR` shift)
