data Expr = Lit Integer | Add Expr Expr deriving Show

eval :: Expr -> Integer
eval (Lit i) = i
eval (Add e e') = (eval e) + (eval e')

printExpr :: Expr -> String
printExpr (Lit i) = show i
printExpr (Add e e') = concat [(printExpr e), " + ", (printExpr e')]
