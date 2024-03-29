{-# language ViewPatterns #-}

import qualified Data.Map        as M
import           Rola
import           Test.Hspec
import           Text.Megaparsec (parseMaybe)

parse :: String -> Maybe Expr
parse = parseMaybe parseExpr

readExpr' :: String -> Expr
readExpr' (readExpr -> Right res) = res

reduceMaybe :: String -> Maybe Expr
reduceMaybe (reduce . readExpr' -> Right res) = Just res
reduceMaybe _ = Nothing

main :: IO ()
main = hspec $ do
  describe "Lam term parsing" $ do
    it "can parse lambda function" $ do
      let res = parse "\\x.x"
      let res' = parse "λx.x"
      res `shouldBe` Just (Lam (Var "x") (Var "x"))
      res' `shouldBe` Just (Lam (Var "x") (Var "x"))

    it "can parse function surrounded by parenthesis" $ do
      let res = parse "(\\x.x)"
      let res' = parse "(λx.x)"
      res `shouldBe` Just (Lam (Var "x") (Var "x"))
      res' `shouldBe` Just (Lam (Var "x") (Var "x"))

    it "can parse curried functions" $ do
      let res = parse "(\\x.\\y.x)"
      let res' = parse "(λx.λy.x)"
      let expec = Just $ Lam (Var "x") (Lam (Var "y") (Var "x"))
      res `shouldBe` expec
      res' `shouldBe` expec

    it "can parse curried functions 2" $ do
      let res = parse "(\\x.\\y.\\z.z)"
      let res' = parse "(λx.λy.λz.z)"
      let expec = Just $ Lam (Var "x") (Lam (Var "y") (Lam (Var "z") (Var "z")))
      res' `shouldBe` expec

  describe "Literals" $ do
    it "can parse bool literals" $ do
      parse "True" `shouldBe` Just (Lit (LBool True))
      parse "False" `shouldBe` Just (Lit (LBool False))

    it "can parse booleans inside functions" $ do
      let res = parse "(\\x.\\y.False)"
      let res' = parse "(\\v.\\w.True)"
      let expec = Lam "x" (Lam "y" (Lit (LBool False)))
      let expec' = Lam "v" (Lam "w" (Lit (LBool True)))
      res `shouldBe` Just expec
      res' `shouldBe` Just expec'

  describe "Lambda application" $ do
    let true = "(λx.λy.x)"
    let false = "(λx.λy.y)"
    let ifte = "(λcond.λt.λe. cond t e)"

    it "identity function" $ do
      reduceMaybe "(\\x.x) 3" `shouldBe` Just (Lit (LInt 3))

    it "fails on undefined variable" $ do
      reduceMaybe "(λx.y) 3" `shouldBe` Nothing

    it "ifThenElse combinator" $ do
      let res = unwords [ifte, true, "3 5"]
      let res' = unwords [ifte, false, "3 5"]
      reduceMaybe res `shouldBe` Just (Lit (LInt 3))
      reduceMaybe res' `shouldBe` Just (Lit (LInt 5))

    it "ifThenElse return functions" $ do
      let res = unwords [ifte, true, "(\\f.\\g.f) (\\v.\\w.w)"]
      let res' = unwords [ifte, false, "(\\f.\\g.f) (\\v.\\w.w)"]
      reduceMaybe res `shouldBe`
        Just (Cls "f" (Lam "g" (Var "f")) churchEncodings)
      reduceMaybe res' `shouldBe`
        Just (Cls "v" (Lam "w" (Var "w")) churchEncodings)
