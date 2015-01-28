module Eval2 where

import Syntax
import EvalTypes

import Control.Monad.State
import Control.Monad.Writer
import Data.Map (Map)
import qualified Data.Map as Map
import Text.PrettyPrint

-- -----------------------------------------------------------------------------

type E a = StateT (Heap,Costs,Int) (Writer [String]) a

trace :: String -> E ()
trace s = tell [s]

-- -----------------------------------------------------------------------------
-- The evaluator

-- This is a rendering of the lazy evaluation "natural semantics"
-- using a state monad to keep track of the heap.  It is very similar
-- to the cost centre semantics in Sansom's thesis (p. 47), the
-- differences being:
--
-- 	* pass/return cost centre *stacks*
--
--	* the SUB function in the variable rule has been replaced by
--	  funCall for lambdas, and for thunks we always set the Stack
--	  for evaluation.  We never subsume the costs of a thunk-
--	  this was implicit in Sansom's semantics because there were
--	  never any thunks with a "SUB" cost centre, but it is explicit
--	  in ours).
--
--	* The scc rule *pushes* the Label on the current Stack.

eval :: Stack -> Expr -> E (Stack,Expr)
eval ccs (EVar x) = eval_var ccs x 

eval ccs (EInt i) =
   return (ccs, EInt i)

eval ccs (ELam x e) =
   return (ccs, ELam x e)

eval ccs (ELet (x,e1) e2) = do 
   trace ("Heap closure for " ++ x ++ ", Stack = " ++ showCCS ccs)
   modifyHeap (\h -> Map.insert x (ccs,e1) h)
   eval ccs e2

eval ccs (EPlus e1 e2) = do
   (_,EInt x) <- eval ccs e1
   (_,EInt y) <- eval ccs e2
   tick ccs
   return (ccs, EInt (x+y))

eval ccs (EApp f x) = do
   (lam_ccs, ELam y e) <- eval ccs f
   eval lam_ccs (subst y x e)

eval ccs (EPush cc e) =
   eval (pushCC cc ccs) e

eval_var ccs x = do
   r <- lookupHeap x
   case r of
	(ccs', EInt i) ->
	   return (ccs', EInt i)

	(ccsv, ELam y e) ->
	   enter ccs ccsv x (ELam y e)

	(ccs',e) -> do 
           trace ("Eval: " ++ x)
	   modifyHeap (\h -> Map.delete x h)
		-- delete it from the heap so we can get blackholes
	   (ccsv, v) <- eval ccs' e
           trace ("Update: " ++ x ++ ", Stack = " ++ showCCS ccsv)
	   update x (ccsv,v)
	   enter ccs ccsv x v

enter ccs ccsv x (EInt i)   = return (ccsv, EInt i)
enter ccs ccsv x (ELam y e) = do
  let call_ccs = funCall ccs ccsv
  trace ("Call: " ++ x ++ ", lam Stack = " ++ showCCS ccsv ++ ", cur Stack = " ++ showCCS ccs
           ++ ", call Stack = " ++ showCCS call_ccs)
  return (call_ccs, ELam y e)

-- -----------------------------------------------------------------------------
-- Stack operations

funCall ccs_app ccs_lam
  = ccs_app `appendCCS` reverse ccs_lam'
  where
    (ccs_root, ccs_app', ccs_lam') = findCommonAncestor (reverse ccs_app) (reverse ccs_lam)

findCommonAncestor [] bs = ([],[],bs)
findCommonAncestor as [] = ([],as,[])
findCommonAncestor (a:as) (b:bs)
  | a == b    = (a:root, as', bs')
  | otherwise = ([], a:as, b:bs)
  where (root, as', bs') = findCommonAncestor as bs

appendCCS :: Stack -> Stack -> Stack
appendCCS ccs [] = ccs
appendCCS ccs ["CAF"] = ccs
appendCCS ccs (cc:ccs') = pushCC cc (appendCCS ccs ccs')

pushCC cc ccs
  | Just trunc <- findCC cc ccs = trunc
  | otherwise                   = cc:ccs

findCC cc [] = Nothing
findCC cc (cc':ccs)
  | cc == cc'  = Just (cc':ccs)
  | otherwise  = findCC cc ccs

-- -----------------------------------------------------------------------------
-- Misc.

tick ccs = modify (\ (h,c,i) -> (h, Map.insertWith (+) ccs 1 c, i))

modifyHeap f = modify (\(h,c,i) -> (f h, c, i))

lookupHeap :: Var -> E (Stack,Expr)
lookupHeap x = do
   (h,c,i) <- get
   case Map.lookup x h of
	Just z -> return z
	Nothing -> error ("unbound variable " ++ x)

update x val = modifyHeap (\h -> Map.insert x val h)

genSym :: E Int
genSym = do
  (h,c,i) <- get
  put (h,c,i+1)
  return i

isVal (ELam _ _) = True
isVal (EInt _)   = True
isVal _          = False

