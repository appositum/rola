module Church where

encodings :: [(String, String)]
encodings =
  [ ("S", "λf.(λg.(λx.f x (g x)))")
  , ("K", "λx.λy.x")
  , ("I", "λx.x")
  , ("Y", "λf.((λx.f (x x)) (λx.f (x x)))")
  , ("zero", "λf.λx.x")
  , ("one", "λf.λx.f x")
  , ("two", "λf.λx.f (f x)")
  , ("three", "λf.λx.f (f (f x))")
  , ("four", "λf.λx.f (f (f (f x)))")
  , ("five", "λf.λx.f (f (f (f (f x))))")
  ]
