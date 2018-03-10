module LambdaComputability (
) where

import Data.Maybe (fromJust)

import Data.Natural

import LambdaTypes
import LambdaParser

churchNum :: Natural -> LambdaTerm
churchNum n = fromJust . fromString $ "λ f x . " ++ mconcat (replicate intN "f (") ++ "x" ++ replicate intN ')'
  where intN = fromIntegral n