module Preload where

import           Ast
import qualified Data.Map.Strict as Map
import           Eval

definitions :: Map.Map String Expr
definitions
  = Map.fromList
        [ ("suc", Lam "w" (Lam "y" (Lam "x" (App (Var "y") (App (App (Var "w") (Var "y")) (Var "x"))))))
        , ("and", Lam "a" (Lam "b" (App (App (Var "a") (Var "b")) (Lam "x" (Lam "y" (Var "y"))))))
        , ("if_then_else", Lam "x" (Var "x"))
        , ("add", Lam "n" (Lam "m" (Lam "f" (Lam "x" (App (App (Var "n") (Var "f")) (App (App (Var "m") (Var "f")) (Var "x")))))))
        , ("mult", Lam "n" (Lam "m" (Lam "f" (App (Var "n") (App (Var "m") (Var "f"))))))
        , ("iszero", Lam "n" (Lam "x" (Lam "y" (App (App (Var "n") (Lam "z" (Var "y"))) (Var "x")))))
        ]
