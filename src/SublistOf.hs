{-# LANGUAGE LambdaCase #-}
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
        if bitsLeft == 0
          then arbitrary >>= go rest (finiteBitSize @Word64 undefined)
          else
            case encoding `quotRem` 2 of
              (encoding', 0) -> go xs (bitsLeft - 1) encoding'
              (encoding', _) -> (x :) <$> go xs (bitsLeft - 1) encoding'
