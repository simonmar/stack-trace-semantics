module Syntax where

import Text.PrettyPrint

-- -----------------------------------------------------------------------------

type Prog = [Bind]

type Bind = (Var,Expr)

type Var = String

type Label = String

data Expr
  = EVar Var
  | EInt Int
  | ELam Var Expr
  | ELet Bind Expr
  | EPlus Expr Expr
  | EApp Expr Var
  | EPush Label Expr
  deriving Eq

instance Show Expr where
  show e = render (pprExpr e)

-- -----------------------------------------------------------------------------

pprProg :: [Bind] -> Doc
pprProg binds = vcat (map (<> semi) (map pprBind binds))

pprBind :: Bind -> Doc
pprBind (x,e) = sep [ text x <+> char '=', nest 2 (pprExpr e) ]

pprExpr :: Expr -> Doc
pprExpr (EVar x) = text x
pprExpr (EInt i) = int i
pprExpr (ELam x e) = char '\\' <> text x <> char '.' <+> pprExpr e
pprExpr (ELet b e2) = sep [ text "let", nest 2 (pprBind b), text "in",
			    nest 2 (pprExpr e2) ]
pprExpr (EPlus e1 e2) = pprExpr2 e1 <+> text "+" <+> pprExpr e2
pprExpr (EApp e x) = pprExpr2 e <+> text x
pprExpr (EPush x e) = sep [ text "scc" <+> text x, nest 2 (pprExpr e) ]

pprExpr2 e@ELet{}  = parens (pprExpr e)
pprExpr2 e@EPush{}  = parens (pprExpr e)
pprExpr2 e@EPlus{} = parens (pprExpr e)
pprExpr2 e = pprExpr e

pprExpr3 e@ELet{}  = parens (pprExpr e)
pprExpr3 e@EApp{}  = parens (pprExpr e)
pprExpr3 e@EPush{}  = parens (pprExpr e)
pprExpr3 e@EPlus{} = parens (pprExpr e)
pprExpr3 e = pprExpr e

subst :: Var -> Var -> Expr -> Expr
subst x y (EVar z)	= EVar (substVar x y z)
subst x y (EInt i)	= EInt i
subst x y (ELam z e)	= ELam z (subst x y e)  -- ToDo: name capture?
subst x y (ELet (z,e1) e2) = ELet (z,subst x y e1) (subst x y e2)
subst x y (EPlus e1 e2)  = EPlus (subst x y e1) (subst x y e2)
subst x y (EApp f z)	= EApp (subst x y f) (substVar x y z)
subst x y (EPush cc e)	= EPush cc (subst x y e)

substVar x y z
  | z == x 		= y
  | otherwise		= z
