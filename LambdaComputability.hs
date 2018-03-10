module LambdaComputability (
) where

import Data.Maybe (fromJust)

import Data.Natural

import LambdaTypes
import LambdaParser
import LambdaTerms

unsafeRead :: String -> LambdaTerm
unsafeRead = fromJust . fromString

churchNum :: Natural -> LambdaTerm
churchNum n = unsafeRead $ "λ f x . " ++ mconcat (replicate intN "f (") ++ "x" ++ replicate intN ')'
  where intN = fromIntegral n

true :: LambdaTerm
true = unsafeRead "λ x y . x"

false :: LambdaTerm
false = unsafeRead "λ x y . y"

if' :: LambdaTerm
if' = unsafeRead "λ f x y . f x y"

eq0 :: LambdaTerm
eq0 = subs true "t" . subs false "f" $ unsafeRead "λ x . x (λ y . f) t"