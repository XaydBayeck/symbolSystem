import Lib
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
      (show $ testExpr !! 0) `shouldBe` "a"
      (show $ testExpr !! 1) `shouldBe` "(+ 1 3)"
      (show $ testExpr !! 2) `shouldBe` "(* (+ a b) (* x y) (- z))"

    it "parseComma test" $ do
      parseComma "" `shouldBe` Nothing
      parseComma "(+ a b)" `shouldBe` Just ("", "+ a b", "")
      parseComma "as ) d" `shouldBe` Just ("as ) d", "", "")
      parseComma "as d" `shouldBe` Just ("as d", "", "")
      parseComma "(- (* x y)) a b s" `shouldBe` Just ("", "- (* x y)", " a b s")
      parseComma "(* (+ a b) (* x y))" `shouldBe` Just ("", "* (+ a b) (* x y)", "")
      parseComma "(+ a b) (* x y)" `shouldBe` Just ("", "+ a b", " (* x y)")

    it "parseSpace test" $ do
      parseSpace "" `shouldBe` Nothing
      parseSpace "a b c" `shouldBe` Just ("a", "b c")
      parseSpace "asd dsa" `shouldBe` Just ("asd", "dsa")
      parseSpace "* (+ a b) (* x y)" `shouldBe` Just ("*", "(+ a b) (* x y)")

    it "parseExpr test" $ do
      let Just expr = parseExpr "a"
       in show expr `shouldBe` "a"
      let Just expr = parseExpr "(- z)"
       in show expr `shouldBe` "(- z)"
      let Just expr = parseExpr "(+ a b)"
       in show expr `shouldBe` "(+ a b)"
      let Just expr = parseExpr "(* (+ a b) (* x y))"
       in show expr `shouldBe` "(* (+ a b) (* x y))"
