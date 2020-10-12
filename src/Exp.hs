module Exp where

import Data.List
import Helpers

type Exs = [Expression]
data Expression = E Int Chain

type Chain = [Var]
data Var = Var Char (Maybe Int) | Id
  deriving (Eq)

instance Eq Expression where
  (E q0 c0) == (E q1 c1) = c0 == c1

instance Show Expression where
  show (E q c) 
    | q > 0     = "+" <> show q <> concatMap show c
    | otherwise = show q <> concatMap show c

instance Show Var where
  show (Var n (Just e)) = [n] <> "^" <> show e
  show (Var n Nothing)  = [n]
  show Id               = ""

cmpVar :: Var -> Var -> Bool
cmpVar (Var n0 _) (Var n1 _) = n0 == n1 
cmpVar Id Id                 = True
cmpVar _ _                   = False

combineChains :: Chain -> Chain -> Chain
combineChains as bs = foldl aux bs as
  where 
    aux :: Chain -> Var -> Chain
    aux cs Id = cs
    aux cs v@(Var n0 exp0)
      | contained = bump v cs [] False
      | otherwise = v : cs 
      where contained = any (cmpVar v) cs
    
    bump :: Var -> Chain -> Chain -> Bool -> Chain
    bump v [] ps False                 = reverse $ v : ps
    bump _ [] ps True                  = reverse ps
    bump v@(Var n0 exp0) (c@(Var n1 exp1):cs) ps bumped
      | n0 == n1 && not bumped         = bump v cs (Var n0 (Just (maybeOr exp0 1 + maybeOr exp1 1)) : ps) True
      | otherwise                      = bump v cs (c : ps) bumped
    bump v@(Var n e) (Id:cs) ps bumped = bump v cs ps bumped
    bump Id (v@(Var n e):cs) ps bumped = bump v cs (v:ps) bumped

combineExpressions :: Expression -> Expression -> Expression
combineExpressions (E q0 c0) (E q1 c1) = E (q0 * q1) (c0 `combineChains` c1)

multiplyExs :: Exs -> Exs -> Exs
multiplyExs as bs = concatMap (\a -> map (combineExpressions a) bs) as

addExpressions :: Exs -> Exs
addExpressions es = aux es []
  where 
    aux [] ps     = reverse ps
    aux (e:es) ps
      | contained = aux (map (auxAdd e) es) ps
      | otherwise = aux es (e : ps)
      where contained = any (==e) es
    -- If the expressions don't match, the 2nd expression is returned
    auxAdd e0@(E q0 c0) e1@(E q1 c1)
      | e0 == e1  = E (q0 + q1) c0
      | otherwise = e1
  
speakExs :: Exs -> String
speakExs exs = intercalate " " (map show exs)