import SymbolSystem
import Test.Hspec
import Test.QuickCheck

testExpr :: [Expression]
testExpr =
  [ Variable $ Sym "a",
    Expr (Op "+") $ Cons (Variable $ Sym "1") $ Single $ Variable $ Sym "3",
    Expr (Op "*") $ Cons aPlusb $ Cons xTimy $ Single negz
  ]

aPlusb = Expr (Op "+") $ Cons (Variable $ Sym "a") $ Single $ Variable $ Sym "b"

xTimy = Expr (Op "*") $ Cons (Variable $ Sym "x") $ Single $ Variable $ Sym "y"

negz = Expr (Op "-") $ Single $ Variable $ Sym "z"

main :: IO ()
main = hspec $ do
  describe "symbolSystem.Expression" $ do
    it "Expression show way return the expression : a, (+ 1 3), (* (+ a b) (* x y) (- z))" $ do
      show (head testExpr) `shouldBe` "a"
      show (testExpr !! 1) `shouldBe` "(+ 1 3)"
      show (testExpr !! 2) `shouldBe` "(* (+ a b) (* x y) (- z))"
