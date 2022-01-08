import SymbolSystem.ExprData
import SymbolSystem.ExprParser
import SymbolSystem.ParserMonad
import Test.Hspec
import Test.QuickCheck

testExpr :: [Expression]
testExpr =
  [ Variable $ Sym "a",
    Expr (Op "+") $ cons (Variable $ Sym "1") $ Single $ Variable $ Sym "3",
    Expr (Op "*") $ cons aPlusb $ cons xTimy $ Single negz
  ]

aPlusb = Expr (Op "+") $ cons (Variable $ Sym "a") $ Single $ Variable $ Sym "b"

xTimy = Expr (Op "*") $ cons (Variable $ Sym "x") $ Single $ Variable $ Sym "y"

negz = Expr (Op "-") $ Single $ Variable $ Sym "z"

main :: IO ()
main = hspec $ do
  describe "symbolSystem.ExprData" $ do
    it "Expression show way return the expression : a, (+ 1 3), (* (+ a b) (* x y) (- z))" $ do
      show (head testExpr) `shouldBe` "a"
      show (testExpr !! 1) `shouldBe` "(+ 1 3)"
      show (testExpr !! 2) `shouldBe` "(* (+ a b) (* x y) (- z))"

  describe "SymbolSystem.ExprParser" $ do
    it "parse string to expression" $ do
      parse opP "(sum " `shouldBe` Res "" (Op "sum")
      parse variable "input" `shouldBe` Res "" (Sym "input")
      let expr1 = item $ parse exprP "(* (+ a b) (* x y) (- z))"
       in show expr1 `shouldBe` "(* (+ a b) (* x y) (- z))"
      let exprs1 = item $ parse exprsP "(+ a b) (* x y) (- z)"
       in show exprs1 `shouldBe` "(+ a b) :> (* x y) :> (- z)"
