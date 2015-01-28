module Main where

import Parser
import Syntax
import Lexer
import qualified Eval
import qualified Eval2
import qualified Eval3
import qualified Eval4
-- import EvalDebug2
import EvalTypes

import qualified Oper1
import qualified Oper2
import qualified Oper3

import Control.Applicative
import System.Environment
import Text.PrettyPrint
import Control.Monad.State
import Control.Monad.Writer
import Data.Map (Map)
import qualified Data.Map as Map
import System.Exit
import Text.Printf

main = do
  [ file ] <- getArgs
  run file  

run :: FilePath -> IO ()
run file = do
  line
  putStrLn ("test: " ++ file)
  line
  s <- readFile file
  let
	prog = parse (alexScanTokens s)
  -- in
  putStrLn "prog:"
  putStrLn (render (pprProg prog))

  let 
        init_heap = Map.fromList [ (x, (cafStack,e)) | (x,e) <- prog ]

	top_expr = ELet ("x", EInt 0) (EApp (EVar "main") "x")

        (((ccs,result), (final_heap, final_costs,_)), msgs) =
                runWriter $ runStateT (Eval.eval topStack top_expr) (init_heap,emptyCosts,1)

        ((oper1_result, (_, oper1_costs,_)), oper1_msgs) =
                runWriter $ runStateT (Oper1.eval topStack top_expr) (init_heap,emptyCosts,1)

        ((oper2_result, (_, oper2_costs,_)), oper2_msgs) =
                runWriter $ runStateT (Oper2.eval topStack top_expr) (init_heap,emptyCosts,1)

        ((oper3_result, (_, oper3_costs,_)), oper3_msgs) =
                runWriter $ runStateT (Oper3.eval topStack top_expr) (init_heap,emptyCosts,1)
  -- in
  putStr "\n"
  putStrLn "result (eval):"
  putStrLn (render (pprExpr result))
  when (oper1_result /= result) $ do
	putStrLn ("*** ERROR: oper1 result wrong: " ++ 
		render (pprExpr oper1_result))
	exitWith (ExitFailure 1)
  when (oper2_result /= result) $ do
	putStrLn ("*** ERROR: oper2 result wrong: " ++ 
		render (pprExpr oper2_result))
	exitWith (ExitFailure 1)
  when (oper3_result /= result) $ do
	putStrLn ("*** ERROR: oper3 result wrong: " ++ 
		render (pprExpr oper3_result))
	exitWith (ExitFailure 1)
  putStr "\n"
  putStrLn "costs (eval):"
  putStrLn (showCosts final_costs)
  when (oper1_costs /= final_costs) $ do
	putStrLn ("*** ERROR: oper1 costs wrong: " ++ showCosts oper1_costs)
	exitWith (ExitFailure 1)
  when (oper2_costs /= final_costs) $ do
	putStrLn ("*** ERROR: oper2 costs wrong: " ++ showCosts oper2_costs)
	exitWith (ExitFailure 1)
  when (oper3_costs /= final_costs) $ do
	putStrLn ("*** ERROR: oper3 costs wrong: " ++ showCosts oper3_costs)
	exitWith (ExitFailure 1)
  putStr "\n\n"

line = putStrLn "-------------------------------------"

showCosts = render . hsep . punctuate (text ",") . map showCost . Map.toList
showCost (ccs,i) = pprStack ccs <+> char '=' <+> int i

-----------------------------------------------------------------------------
-- Different semantics

type Evaluator = [Bind] -> (Costs, [String])

----------
-- eval1 is GHC <= 7.2's cost-centre-stack semantics, where
--
--   funCall ccs lam_ccs
--     | not (null lam_ccs) && last lam_ccs == "CAF" = ccs `appendCCS` lam_ccs
--     | otherwise                                   = lam_ccs

eval1 :: Evaluator
eval1 prog = (final_costs, msgs)
  where
    (((ccs,result), (final_heap, final_costs, _)), msgs) =
       runWriter $ runStateT (Eval.eval topStack topExpr)
                             (mkInitHeap prog, emptyCosts, 1)

----------
-- eval2 is GHC 7.4's cost-centre-stack semantics, where
--
--   funCall ccs_app ccs_lam
--     = ccs_app `appendCCS` reverse ccs_lam'
--     where
--       (ccs_root, ccs_app', ccs_lam') =
--           findCommonAncestor (reverse ccs_app) (reverse ccs_lam)

eval2 :: Evaluator
eval2 prog = (final_costs, msgs)
  where
    (((ccs,result), (final_heap, final_costs, _)), msgs) =
       runWriter $ runStateT (Eval2.eval topStack topExpr)
                             (mkInitHeap prog, emptyCosts, 1)

----------
-- eval3 is the failed experiment:
--
--    funCall ccs_app ccs_lam = ccs_app
--
-- together with a preprocessing phase that duplicates CCSs into
-- nested lambdas.

eval3 :: Evaluator
eval3 prog = (final_costs, msgs)
  where
    (((ccs,result), (final_heap, final_costs, _)), msgs) =
       runWriter $ runStateT (Eval3.eval topStack (Eval3.preproc [] topExpr))
                             (mkInitHeap [ (x,Eval3.preproc [] e)
                                         | (x,e) <- prog ], emptyCosts, 1)

----------
-- eval4 is like eval2, but with
--
--   funCall ccs_app ccs_lam
--     = ccs_app `appendCCS` ccs_lam
--
--   pushCC cc ccs
--     | Just _ <- findCC cc ccs = ccs
--     | otherwise               = cc:ccs

eval4 :: Evaluator
eval4 prog = (final_costs, msgs)
  where
    (((ccs,result), (final_heap, final_costs, _)), msgs) =
       runWriter $ runStateT (Eval4.eval topStack topExpr)
                             (mkInitHeap prog, emptyCosts, 1)

-----------------------------------------------------------------------------

parseFile :: FilePath -> IO [Bind]
parseFile testf = do
  s <- readFile testf
  return (parse (alexScanTokens s))

mkInitHeap :: [Bind] -> Heap
mkInitHeap prog = Map.fromList [ (x, (cafStack,e)) | (x,e) <- prog ]

topExpr :: Expr
topExpr = ELet ("x", EInt 0) (EApp (EVar "main") "x")

maxTest = 14

runTest :: ([Bind] -> (Costs,[String])) -> FilePath -> IO ()
runTest top_eval f = do
     c <- fst <$> top_eval <$> parseFile f
     printf "%10s : %s\n" f (showCosts c)

allTests :: ([Bind] -> (Costs,[String])) -> IO ()
allTests top_eval =
  forM_ [ 1.. maxTest ] $ \n -> do
     let f = "test" ++ show n
     runTest top_eval f

debugTest :: FilePath -> ([Bind] -> (Costs,[String])) -> IO ()
debugTest f top_eval = do
  bs <- parseFile f
  let (costs, msgs) = top_eval bs
  mapM_ putStrLn msgs
  putStrLn (showCosts costs)

allTests1 = allTests eval1
allTests2 = allTests eval2
allTests3 = allTests eval3
allTests4 = allTests eval4

versions = [eval1, eval2, eval3, eval4]

cmpTest :: FilePath -> IO ()
cmpTest f = do
     binds <- parseFile f
     let rs = map ($ binds) versions
     printf "%10s :\n" f
     forM_ (zip [1..] rs) $ \(n,r) -> do
        printf "%-7d : %s\n" (n::Int) (showCosts (fst r))

cmpTests =
  forM_ [ 1.. maxTest ] $ \n ->
     cmpTest ("test" ++ show n)

tests_Eval =
  [("test1",Map.fromList [(["f","TOP"],1)]),
   ("test2",Map.fromList [(["double","main","TOP"],1)]),
   ("test3",Map.fromList [(["double","f2","f1","main","TOP"],1)]),
   ("test4",Map.fromList [(["double","g","main","TOP"],1)]),
   ("test5",Map.fromList [(["double","g","CAF"],1),(["double","g","main","TOP"],1),(["g","main","TOP"],1)]),
   ("test6",Map.fromList [(["double","g","CAF"],1)]),
   ("test7",Map.fromList [(["double","z","g","main","TOP"],1)]),
   ("test8",Map.fromList [(["double","j","g","main1","main","TOP"],1)]),
   ("test9",Map.fromList [(["double","g","main","TOP"],1)]),
   ("test10",Map.fromList [(["double","g","main1","main","TOP"],1)]),("test11",Map.fromList [(["double","g1","g","main1","main","TOP"],1)]),("test12",Map.fromList [(["double","g","main","TOP"],1)]),("test13",Map.fromList [(["double","f","main","TOP"],1)]),("test14",Map.fromList [(["double","g","main1","main","TOP"],1)])]

