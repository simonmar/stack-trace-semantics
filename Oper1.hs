module Oper1 (eval) where

import Syntax
import EvalTypes
import Eval hiding (eval)

import Control.Monad.State
import Data.Map (Map)
import qualified Data.Map as Map
import Text.PrettyPrint

eval :: Stack -> Expr -> E Expr
eval = oper []

-- -----------------------------------------------------------------------------
-- Operational semantics

-- Now we want to refine the semantics to be more operational; more
-- like the abstract machine.
--
-- First step: we have an explicit stack.  Proceed by transforming
-- eval, replacing each 'return' by a function 'ret' which examines the
-- stack for what to do next, and replace each non-tail call to eval by a
-- tail call with an explicit continuation pushed on the stack.
--
-- Because this is a mechanical transformation of eval, it is trivially
-- an implementation of the semantics.

-- The operational semantics is completely tail-recursive, so can be
-- implemented as an interpretive loop, or with compiled code.  The
-- stack, current Stack, cost mapping and heap are all single-threaded,
-- so can be implemented as mutable globals.

type VMStack = [Continuation]

data Continuation
  = ApplyTo Var
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
  oper (Plus1 e2 ccs : stack) ccs e1
  
oper stack ccs (EApp f x) =
  oper (ApplyTo x : stack) ccs f 

oper stack ccs (EPush cc e) =
  oper stack (pushCC cc ccs) e

ret :: VMStack -> Stack -> Expr -> E Expr
ret [] ccs v = return v
ret (ApplyTo x : stack) ccs (ELam y e) = 
  oper stack ccs (subst y x e)
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

-- What this *doesn't* tell us: can the update operation be implemented
-- by indirection?  Next refinement: be explicit about object identity
-- and copying.
