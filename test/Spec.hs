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
      res `shouldBe` Just (Abs "x" (Var "x"))
      res' `shouldBe` Just (Abs "x" (Var "x"))

    it "can parse function surrounded by parenthesis" $ do
      let res = parse "(\\x.x)"
      let res' = parse "(λx.x)"
      res `shouldBe` Just (Abs "x" (Var "x"))
      res' `shouldBe` Just (Abs "x" (Var "x"))

    it "can parse curried functions" $ do
      let res = parse "(\\x.\\y.x)"
      let res' = parse "(λx.λy.x)"
      res `shouldBe` Just (Abs "x" (Abs "y" (Var "x")))
      res' `shouldBe` Just (Abs "x" (Abs "y" (Var "x")))

    it "can parse curried functions 2" $ do
      let res = parse "(\\x.\\y.\\z.z)"
      let res' = parse "(λx.λy.λz.z)"
      res `shouldBe` Just (Abs "x" (Abs "y" (Abs "z" (Var "z"))))
      res' `shouldBe` Just (Abs "x" (Abs "y" (Abs "z" (Var "z"))))

  describe "Literals" $ do
    it "can parse bool literals" $ do
      parse "true" `shouldBe` Just (Literal (LBool True))
      parse "false" `shouldBe` Just (Literal (LBool False))

    it "can parse booleans inside functions" $ do
      let res = parse "(\\x.\\y.false)"
      let res' = parse "(\\v.\\w.true)"
      res `shouldBe` Just (Abs "x" (Abs "y" (Literal (LBool False))))
      res' `shouldBe` Just (Abs "v" (Abs "w" (Literal (LBool True))))
