module Oper2 (eval) where

import Syntax
import qualified Eval
import Eval hiding (eval)
import EvalTypes

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Text.PrettyPrint

eval :: Stack -> Expr -> E Expr
eval = oper []

-- -----------------------------------------------------------------------------
-- Next step?

-- Now, we move to an operational semantics that deals with multiple arguments
-- simultaneously.

-- We simply treat multiple nested EApp as an n-ary application, and
-- multiple nested ELams as an n-ary function.

type VMStack = [Continuation]

data Continuation
  = ApplyTo [Var]
  | Update Var Stack
  | Plus1 Expr Stack
  | Plus2 Int  Stack
  deriving (Show)


oper :: VMStack -> Stack -> Expr -> E Expr

oper stack ccs (EVar x) = do
   r <- lookupHeap x
   case r of
	(ccs', ELam y e) ->
	   ret stack (funCall ccs ccs') (ELam y e)

	(ccs',e) -> do
	   modifyHeap (\h -> Map.delete x h)
		-- delete it from the heap so we can get blackholes
	   oper (Update x ccs : stack) ccs' e

oper stack ccs (EInt i) =
  ret stack ccs (EInt i)

oper stack ccs (ELam x e) = 
  ret stack ccs (ELam x e)

oper stack ccs (ELet (x,e1) e2) = do
  modifyHeap (\h -> Map.insert x (ccs,e1) h)
  oper stack ccs e2

oper stack ccs (EPlus e1 e2) = do
  (_,EInt x) <- Eval.eval ccs e1  -- TODO!
  (_,EInt y) <- Eval.eval ccs e2
  tick ccs
  ret stack ccs (EInt (x+y))
  
oper stack ccs e@(EApp _ _) = do
  let (f,xs) = collectArgs e []
  oper (ApplyTo xs : stack) ccs f 

oper stack ccs (EPush cc e) =
  oper stack (pushCC cc ccs) e


ret [] ccs v = return v
ret (ApplyTo xs : stack) ccs e@(ELam _ _) = do
  let (ys,body) = collectLams e
  case () of
      _ | length xs >  length ys -> do
		let (xs1,xs2) = splitAt (length ys) xs
		oper (ApplyTo xs2 : stack) ccs
			(foldr (.) id (zipWith subst ys xs1) body)
	| length xs == length ys ->
		oper stack ccs (foldr (.) id (zipWith subst ys xs) body)
	| otherwise ->
		ret stack ccs 
		  (dropLams (foldr (.) id (zipWith subst ys xs) e)
			    (length xs))

ret (Update x ccs : stack) ccsv v = do
  update x (ccsv,v)
  case v of
	ELam y e -> ret stack (funCall ccs ccsv) v
	_        -> ret stack ccsv v
ret (Plus1 e ccs : stack) _ (EInt x) =
  oper (Plus2 x ccs : stack) ccs e
ret (Plus2 x ccs : stack) _ (EInt y) = do
  tick ccs
  ret stack ccs (EInt (x+y))
ret stack ccs v = 
  error ("ret: " ++ show (head stack) ++ ", " ++ render (pprExpr v))


collectArgs (EApp f x) xs = collectArgs f (x:xs)
collectArgs e xs = (e,xs)

collectLams (ELam y b) = let (ys,b') = collectLams b in (y:ys, b')
collectLams e = ([],e)

dropLams e 0 = e
dropLams (ELam _ body) n = dropLams body (n-1)
dropLams e _ = error ("dropLams: " ++ render (pprExpr e))
