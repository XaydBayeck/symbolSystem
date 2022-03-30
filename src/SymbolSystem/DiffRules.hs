module SymbolSystem.DiffRules where

import SymbolSystem.ExprData

diff :: Expression -> Symbol -> Expression
diff (Variable u) x = if u == x then Variable $ Sym "1" else Variable $ Sym "0"
diff (Expr (Op "*") (e :> es)) x = Expr (Op "+") (e1 :> Single e2)
  where
    e1 = Expr (Op "*") (diff e x :> Single des)
    e2 = Expr (Op "*") (diff des x :> Single e)
    des = case es of
      Single e -> e
      est@(e :> ess) -> Expr (Op "*") est
diff (Expr (Op "+") (e :> es)) x = Expr (Op "+") des
  where
    des = helper (Single $ diff e x) es
    helper res (e :> es) = helper (diff e x :> res) es
    helper res (Single e) = diff e x :> res
diff expr x = Expr (Op "diff") (expr :> Single (Variable x))

(<~|->) = diff
