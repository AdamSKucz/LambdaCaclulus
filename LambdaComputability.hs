module LambdaComputability (
  churchNum,
  projF,
  zeroF,
  succF
) where

import Data.Maybe (fromJust)

import Data.Natural

import LambdaTypes
import LambdaUtil
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

pred' :: LambdaTerm
pred' = subs snd' "Snd" . subs pair "Pair" . subs g "G" $ unsafeRead "λ y f x . Snd (y (G f) (Pair x x))"
  where g = subs fst' "Fst" $ unsafeRead "λ f p . Pair (f (Fst p)) (Fst p)"

yCombinator :: LambdaTerm
yCombinator = unsafeRead "λ f . (λ x . f (x x)) (λ x . f (x x))"

ifThenElse :: LambdaTerm -> LambdaTerm -> LambdaTerm -> LambdaTerm
ifThenElse condition iftrue iffalse = App (App (App if' condition) iftrue) iffalse

-- functions for partial recursive set
projF :: Natural -> Natural -> LambdaTerm
projF n i = unsafeRead $ "λ" ++ mconcat [" x"++show i | i <- [1..n]] ++ " . x" ++ show i

zeroF :: Natural -> LambdaTerm
zeroF = const $ churchNum 0

succF :: LambdaTerm
succF = unsafeRead "λ n . λ f x . f (n f x)"

idF :: LambdaTerm
idF = unsafeRead "λ x . x"

composeF :: Natural -> Natural -> LambdaTerm -> [LambdaTerm] -> LambdaTerm
composeF m n fLT gLTs = bindXs $ App gChecks appliedF
  where xVars :: [Var]
        xVars = getNNewVars (fromIntegral m) (fLT:gLTs)
        bindXs :: LambdaTerm -> LambdaTerm
        bindXs lt = foldr Lambda lt xVars
        applyToXs :: LambdaTerm -> LambdaTerm
        applyToXs = applyToAll [V x | x <- xVars]
        appliedGs :: [LambdaTerm]
        appliedGs = [applyToXs g | g <- gLTs]
        gChecks :: LambdaTerm
        gChecks = applyToAll [App gVal idF | gVal <- appliedGs] idF
        appliedF :: LambdaTerm
        appliedF = applyToAll appliedGs fLT

primRecF :: Natural -> LambdaTerm -> LambdaTerm -> LambdaTerm
primRecF n f g =  App yCombinator . 
                  Lambda h . 
                  bindXs .
                  ifThenElse (App eq0 $ V x) (applyToXs f) .
                  App (App (applyToXs g) xPred) $
                  App (applyToXs $ V h) xPred
  where (h:x:vars) = getNNewVars (2 + fromIntegral n) [f,g]
        bindXs :: LambdaTerm -> LambdaTerm
        bindXs lt = foldr Lambda lt vars
        applyToXs :: LambdaTerm -> LambdaTerm
        applyToXs = applyToAll [V y | y <- vars]
        xPred :: LambdaTerm
        xPred = App pred' $ V x