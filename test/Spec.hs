import Rola
import Test.Hspec
import Text.Megaparsec (parseMaybe)

parse :: String -> Maybe Expr
parse = parseMaybe parseExpr

main :: IO ()
main = hspec $ do
  describe "Lambda term parsing" $ do
    it "can parse lambda function" $ do
      let res = parse "\\x.x"
      let res' = parse "λx.x"
      res `shouldBe` Just (Abs (Var "x") (Var "x"))
      res' `shouldBe` Just (Abs (Var "x") (Var "x"))

    it "can parse function surrounded by parenthesis" $ do
      let res = parse "(\\x.x)"
      let res' = parse "(λx.x)"
      res `shouldBe` Just (Abs (Var "x") (Var "x"))
      res' `shouldBe` Just (Abs (Var "x") (Var "x"))

    it "can parse curried functions" $ do
      let res = parse "(\\x.\\y.x)"
      let res' = parse "(λx.λy.x)"
      res `shouldBe` Just (Abs (Var "x") (Abs (Var "y") (Var "x")))
      res' `shouldBe` Just (Abs (Var "x") (Abs (Var "y") (Var "x")))

    it "can parse curried functions 2" $ do
      let res = parse "(\\x.\\y.\\z.z)"
      let res' = parse "(λx.λy.λz.z)"
      let expec = Just $ Abs (Var "x") (Abs (Var "y") (Abs (Var "z") (Var "z")))
      res' `shouldBe` expec

  describe "Literals" $ do
    it "can parse bool literals" $ do
      parse "true" `shouldBe` Just (Literal (LBool True))
      parse "false" `shouldBe` Just (Literal (LBool False))

    it "can parse booleans inside functions" $ do
      let res = parse "(\\x.\\y.false)"
      let res' = parse "(\\v.\\w.true)"
      let expec = Abs (Var "x") (Abs (Var "y") (Literal (LBool False)))
      let expec' = Abs (Var "v") (Abs (Var "w") (Literal (LBool True)))
      res `shouldBe` Just expec
      res' `shouldBe` Just expec'
