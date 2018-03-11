module LambdaComputability (
  churchNum,
  projF,
  zeroF,
  succF
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

pair :: LambdaTerm
pair = unsafeRead "λ x y f . f x y"

fst' :: LambdaTerm
fst' = subs true "t" $ unsafeRead "λ f . f t"

snd' :: LambdaTerm
snd' = subs false "t" $ unsafeRead "λ f . f t"

-- functions for partial recursive set
projF :: Natural -> Natural -> LambdaTerm
projF n i = unsafeRead $ "λ" ++ mconcat [" x"++show i | i <- [1..n]] ++ " . x" ++ show i

zeroF :: Natural -> LambdaTerm
zeroF = const $ churchNum 0

succF :: LambdaTerm
succF = unsafeRead "λ n . λ f x . f (n f x)"

composeF :: Natural -> Natural -> LambdaTerm -> [LambdaTerm] -> LambdaTerm
composeF m n fLT gLTs = undefined