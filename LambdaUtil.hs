module LambdaUtil (
  foldLT,
  subsVar,
  getNewVar
) where 

import Data.Set (Set, union, notMember)
import qualified Data.Set as Set

import LambdaTypes

foldLT :: (Var -> a) -> (Var -> a -> a) -> (a -> a -> a) -> LambdaTerm -> a
foldLT v l a = vlaFoldLT
  where vlaFoldLT (V x)       = v x
        vlaFoldLT (Lambda x m)  = l x (vlaFoldLT m)
        vlaFoldLT (App m n)     = a (vlaFoldLT m) (vlaFoldLT n)

subsVar :: Var -> Var -> LambdaTerm -> LambdaTerm
subsVar x' x = foldLT (V . replace) (Lambda . replace) App
  where replace y = if y == x then x' else y

variables :: LambdaTerm -> Set Var
variables = foldLT Set.singleton (\v mVars -> mVars `union` Set.singleton v) union

allVars :: String -> [String]
allVars alphabet = [ c : s | s <- "" : allVars alphabet, c <- alphabet]

auxVars :: [String]
auxVars = allVars ['a'..'z']

getNewVar :: LambdaTerm -> Var
getNewVar m = head . filter (`notMember` variables m) $ auxVars