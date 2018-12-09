import Rola
import Test.Hspec
import Text.Megaparsec

parse' p = parse p mempty

toMaybe :: Either a b -> Maybe b
toMaybe (Left _) = Nothing
toMaybe (Right a) = Just a

main :: IO ()
main = hspec $ do
  describe "Lambda term parsing" $ do
    it "can parse lambda abstraction" $ do
      let res = toMaybe $ parse' parseAbs "\\x.x"
      res `shouldBe` Just (Abs "x" (Var "x"))
