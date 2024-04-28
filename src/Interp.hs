module Interp where

import Declare
import Prelude hiding (EQ, GT, LT)

unary :: UnaryOp -> Value -> Value
unary Not (BoolV b) = BoolV (not b)
unary Neg (IntV i) = IntV (- i)
unary _ _ = undefined

binary :: BinaryOp -> Value -> Value -> Value
binary Add (IntV a) (IntV b) = IntV (a + b)
binary Sub (IntV a) (IntV b) = IntV (a - b)
binary Mult (IntV a) (IntV b) = IntV (a * b)
binary Div (IntV a) (IntV b) = IntV (a `div` b)
binary And (BoolV a) (BoolV b) = BoolV (a && b)
binary Or (BoolV a) (BoolV b) = BoolV (a || b)
binary LT (IntV a) (IntV b) = BoolV (a < b)
binary LE (IntV a) (IntV b) = BoolV (a <= b)
binary GE (IntV a) (IntV b) = BoolV (a >= b)
binary GT (IntV a) (IntV b) = BoolV (a > b)
binary EQ a b = BoolV (a == b)
binary _ _ _ = undefined

-- Call-by-need evaluation

evaluate :: Exp -> Env -> (Value, Env)
evaluate (Lit v) env = (v, env)
evaluate (Unary op e) env =
  let (v, env') = evaluate e env
   in (unary op v, env')
evaluate (Bin op e1 e2) env =
  let (v1, env') = evaluate e1 env
      (v2, env'') = evaluate e2 env'
   in (binary op v1 v2, env'')
evaluate (If e1 e2 e3) env =
  let (BoolV a, env') = evaluate e1 env
   in if a then evaluate e2 env' else evaluate e3 env'
evaluate (Var x) env =
  case lookup x env of
    Just (Right v) -> (v, env)
    Just (Left (CExp e env')) ->
      let (v, env'') = evaluate e env'
       in (v, (x, Right v) : env)
    Nothing -> error ("Variable " ++ x ++ " not found")
evaluate (Decl x t e body) env =
  let newEnv = (x, Left (CExp e newEnv)) : (env)
   in evaluate body newEnv
evaluate (Fun (x, t) body) env = (ClosureV (x, t) body env, env)
evaluate (Call fun arg) env =
  let (v, env'') = evaluate body newEnv
        where
          (ClosureV (x, t) body env') = fst (evaluate fun env)
          newEnv = ((x, Left (CExp arg env))) : env'
   in (v, env ++ env'')
evaluate (MultDecl xs body) env =
  let newEnv = foldl (\env' (x, t, e) -> (x, Left (CExp e newEnv)) : env') env xs
   in evaluate body newEnv

execute :: Exp -> Value
execute e = fst (evaluate e [])
