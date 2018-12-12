module Church where

import Rola

fromRight :: Either e a -> a
fromRight (Right a) = a

parse :: String -> Expr
parse = fromRight . readExpr

s = parse "λf.(λg.(λx.f x (g x)))"
k = parse "λx.λy.x"
i = parse "λx.x"
y = parse "λf.((λx.f (x x)) (λx.f (x x)))"

true = parse "λx.λy.x"
false = parse "λx.λy.y"

zero  = parse "λf.λx.x"
one   = parse "λf.λx.f x"
two   = parse "λf.λx.f (f x)"
three = parse "λf.λx.f (f (f x))"
four  = parse "λf.λx.f (f (f (f x)))"
