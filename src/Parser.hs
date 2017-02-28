{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE Rank2Types                #-}
{-# LANGUAGE TypeSynonymInstances      #-}

module Parser where

import           Ast
import           Text.ParserCombinators.UU
import           Text.ParserCombinators.UU.BasicInstances
import           Text.ParserCombinators.UU.Utils

parse :: String -> Expr
parse = runParser "<INPUT>" pExpr

pExpr :: Parser Expr
pExpr = foldl1 App <$> pList1Sep pSpaces pTerm


pTerm :: Parser Expr
pTerm =  pParens pExpr
     <|> pVariable
     <|> pLit
     <|> pLambda

pVariable :: Parser Expr
pVariable = Var <$> pIdentifier

pLit =  Lit . LInt  <$> pInteger
    <|> Lit . LBool <$> lexeme pBoolValue

pLambda :: Parser Expr
pLambda = f <$ pSymbol "\\" <*> pList1Sep pSpaces pIdentifier <* pSymbol "->" <*> pExpr
  where f = (\vars expr -> foldr Lam expr vars)

pBoolValue :: Parser Bool
pBoolValue =     True  <$ pSymbol "True"
             <|> False <$ pSymbol "False"

pIdentifier :: Parser String
pIdentifier = (:) <$> pLower <*> pList (pLower <|> pUpper <|> pDigit <|> pSym '_') <* pSpaces
