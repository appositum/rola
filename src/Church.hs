module Church where

encodings :: [(String, String)]
encodings =
  [ ("S", "λf.(λg.(λx.f x (g x)))")
  , ("K", "λx.λy.x")
  , ("I", "λx.x")
  , ("Y", "λf.((λx.f (x x)) (λx.f (x x)))")
  , ("true", "λx.λy.x")
  , ("false", "λx.λy.y")
  , ("ifte", "λcond.λt.λe.cond t e")
  , ("not", "λcond.λt.λe.cond e t")
  , ("zero", "λf.λx.x")
  , ("one", "λf.λx.f x")
  , ("two", "λf.λx.f (f x)")
  , ("three", "λf.λx.f (f (f x))")
  , ("four", "λf.λx.f (f (f (f x)))")
  , ("five", "λf.λx.f (f (f (f (f x))))")
  , ("succ", "λn.λf.λx.f (n f x)")
  , ("pred", "λn.λf.λx. n (λg.λh. h (g f)) (λu. x) (λu. u)")
  , ("plus", "λm.λn.λf.λx.m f (n f x)")
  , ("mult", "λm.λn.λf.λx.m (n f) x")
  , ("exp", "λm.λn.m n")
  ]
