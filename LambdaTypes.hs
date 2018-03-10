module LambdaTypes (
  Var,
  LambdaTerm(..),
) where

type Var = String
data LambdaTerm = V Var | Lambda Var LambdaTerm | App LambdaTerm LambdaTerm
  deriving (Show, Read, Eq)