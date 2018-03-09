import Data.Set (Set, (\\), union, notMember)
import qualified Data.Set as Set

type Var = String
data LambdaTerm = Var Var | Lambda Var LambdaTerm | App LambdaTerm LambdaTerm
  deriving (Show, Read, Eq)

foldLT :: (Var -> a) -> (Var -> a -> a) -> (a -> a -> a) -> LambdaTerm -> a
foldLT v l a = vlaFoldLT
  where vlaFoldLT (Var x)       = v x
        vlaFoldLT (Lambda x m)  = l x (vlaFoldLT m)
        vlaFoldLT (App m n)     = a (vlaFoldLT m) (vlaFoldLT n)

freeVariables :: LambdaTerm -> Set Var
freeVariables = foldLT Set.singleton (\v mVars -> mVars \\ Set.singleton v) union

boundVariables :: LambdaTerm -> Set Var
boundVariables = foldLT (const Set.empty) (\v mVars -> mVars `union` Set.singleton v) union

subsVar :: Var -> Var -> LambdaTerm -> LambdaTerm
subsVar x' x = foldLT (Var . replace) (Lambda . replace) App
  where replace y = if y == x then x' else y

variables :: LambdaTerm -> Set Var
variables = foldLT Set.singleton (\v mVars -> mVars `union` Set.singleton v) union

allVars :: String -> [String]
allVars alphabet = [ c : s | s <- "" : allVars alphabet, c <- alphabet]

auxVars :: [String]
auxVars = allVars ['a'..'z']

alphaEquiv :: LambdaTerm -> LambdaTerm -> Bool
alphaEquiv (Var x) (Var x')
  | x == x' = True
alphaEquiv (Lambda x m) (Lambda y n)
  | subsVar z x m `alphaEquiv` subsVar z y n = True
  where z = head . filter (`notMember` variables (App m n)) $ auxVars
alphaEquiv (App m n) (App m' n')
  | m `alphaEquiv` m' && n `alphaEquiv` n' = True
alphaEquiv _ _ = False

subs :: LambdaTerm -> Var -> LambdaTerm -> LambdaTerm
subs m x = subsMX
  where subsMX (Var y)
          | y == x          = m
          | otherwise       = Var y
        subsMX (Lambda y n) = Lambda z (subsMX $ subsVar z y n)
          where z = head . filter (`notMember` variables (App m $ Var x)) $ y:auxVars
        subsMX (App n1 n2)  = App (subsMX n1) (subsMX n2)

--oneStepBetaReduce :: LambdaTerm -> Maybe LambdaTerm

--betaEquivalent :: LambdaTerm -> LambdaTerm -> Bool

-- TODO: implement checks for non-existence of bnf
-- scratch the above, that's the Halting Problem ;p
--toBetaNormalForm :: LambdaTerm -> LambdaTerm

