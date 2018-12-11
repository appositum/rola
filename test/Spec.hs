import Rola
import Test.Hspec
import Text.Megaparsec

main :: IO ()
main = hspec $ do
  describe "Lambda term parsing" $ do
    it "can parse lambda abstraction" $ do
      let res = parseMaybe parseAbs "\\x.x"
      res `shouldBe` Just (Abs "x" (Var "x"))

    -- TODO
    -- it "can parse abstraction surrounded by parenthesis" $ do
    --   let res = parseMaybe parseAbs "(\\x.x)"
    --   res `shouldBe` Just (Abs "x" (Var "x"))

  describe "Literals" $ do
    it "can parse bool literals" $ do
      parseMaybe parseBool "true" `shouldBe` Just (Literal (LBool True))
      parseMaybe parseBool "false" `shouldBe` Just (Literal (LBool False))
