module Oper3 (eval) where

import Syntax
import Eval hiding (eval)
import qualified Eval
import EvalTypes

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Text.PrettyPrint
import Debug.Trace

eval :: Stack -> Expr -> E Expr
eval = oper []

-- -----------------------------------------------------------------------------
-- Operational semantics

type VMStack = [Continuation]

data Continuation
  = ApplyTo Var
  | Update Var Stack
  | Plus1 Expr  Stack
  | Plus2 Int  Stack
  deriving (Show)

oper :: VMStack -> Stack -> Expr -> E Expr

oper stack ccs (EVar x) = do
  z <- lookupHeap x
  case z of
	(ccs', ELam y e) -> ret stack ccs x

	(ccs',e) -> do
	   modifyHeap (\h -> Map.delete x h)
		-- delete it from the heap so we can get blackholes
	   oper (Update x ccs : stack) ccs' e


oper stack ccs (EInt i) = do
  x <- newHeapObject ccs (EInt i)
  ret stack ccs x

oper stack ccs (ELam x e) = do
  x <- newHeapObject ccs (ELam x e)
  ret stack ccs x

oper stack ccs (ELet (x,e1) e2) = do
  modifyHeap (\h -> Map.insert x (ccs,e1) h)
  oper stack ccs e2

oper stack ccs (EPlus e1 e2) = do
  oper (Plus1 e2 ccs : stack) ccs e1
  
oper stack ccs (EApp f x) =
  oper (ApplyTo x : stack) ccs f 

oper stack ccs (EPush cc e) =
  oper stack (pushCC cc ccs) e


newHeapObject :: Stack -> Expr -> E Var
newHeapObject ccs e = do
  i <- genSym
  let x = "_x" ++ show i
  update x (ccs,e)
  return x


ret :: VMStack -> Stack -> Var -> E Expr
ret [] ccs z = do
  (ccs,e) <- lookupHeap z
  return e
ret (ApplyTo x : stack) ccs z = do
  (ccs_lam, ELam y e) <- lookupHeap z
  oper stack (funCall ccs ccs_lam) (subst y x e)
ret (Update x ccs_up : stack) ccs z = do
  (ccs_z,v) <- lookupHeap z
  case v of 
    ELam y e -> do
	update x (funCall ccs ccs_z, ELam y e)
		-- If ccs==ccs_z, then this is an indirection.
		-- when is this not true?  Multiple nested update
		-- frames of function type (seq frames?)
	ret stack ccs_up x
    _other -> do
	update x (ccs_z,v) -- an indirection
	ret stack ccs z
		-- why not ccs_up?  we don't care about ccs_up, because the
		-- enclosing frame is either Update or PlusX, and in both
		-- cases the current Stack is ignored.

ret (Plus1 e ccs : stack) _ x = do
  (_, EInt x) <- lookupHeap x
  oper (Plus2 x ccs : stack) ccs e
ret (Plus2 x ccs : stack) _ y = do
  (_, EInt y) <- lookupHeap y
  tick ccs
  z <- newHeapObject ccs (EInt (x+y))
  ret stack ccs z

ret stack ccs v = 
  error ("ret: " ++ show (head stack) ++ ", " ++ render (pprExpr (EVar v)))

-- Fact: funCall(ccs,ccs) == ccs
