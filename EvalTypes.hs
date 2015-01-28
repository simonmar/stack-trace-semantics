module EvalTypes (
        Heap,
        Stack, pprStack, showCCS,
        topStack,
        cafLabel, cafStack,
        Costs, emptyCosts
  ) where

import qualified Data.Map as Map
import Data.Map (Map)
import Syntax
import Text.PrettyPrint
import Control.Monad.State

-----------------------------------------------------------------------------

type Heap = Map Var (Stack,Expr)

-----------------------------------------------------------------------------

type Stack  = [Label]

topStack = cafStack

cafLabel  = "CAF"
cafStack = [cafLabel]

pprStack :: Stack -> Doc
pprStack ccs = char '<' <> hcat (punctuate comma (map text (reverse ccs))) <> char '>'

showCCS = render . pprStack

-----------------------------------------------------------------------------

type Costs = Map Stack Int

emptyCosts :: Costs
emptyCosts = Map.empty

