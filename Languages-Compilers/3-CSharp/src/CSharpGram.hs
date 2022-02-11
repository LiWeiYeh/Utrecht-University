module CSharpGram where

import ParseLib.Abstract hiding (braced, bracketed, parenthesised)
import CSharpLex
import Prelude hiding ((<$), (<*), (*>), sequence)

data Class = Class String [Member]
           deriving Show

data Member = MemberD Decl
            | MemberM Type String [Decl] Stat
            deriving Show

data Stat = StatDecl   Decl
          | StatExpr   Expr
          | StatIf     Expr Stat Stat
          | StatWhile  Expr Stat
          | StatReturn Expr
          | StatBlock  [Stat]
          deriving Show

data Expr = ExprConst  Int
          | ExprVar    String
          | ExprOper   String Expr Expr
          | ExprCall   String [Expr]
          deriving Show

data Decl = Decl Type String
          deriving Show

data Type = TypeVoid
          | TypePrim  String
          | TypeObj   String
          deriving (Eq,Show)


pClass :: Parser Token Class
pClass = Class <$ symbol KeyClass <*> sUpperId <*> braced (many pMember)

pMember :: Parser Token Member
pMember =  MemberD <$> pDeclSemi
       <|> pMeth

pMeth :: Parser Token Member
pMeth = MemberM <$> methRetType <*> sLowerId <*> methArgList <*> pBlock
    where
        methRetType = pType <|> TypeVoid <$ symbol KeyVoid
        methArgList = parenthesised (option (listOf pDecl (symbol Comma)) [])

pBlock :: Parser Token Stat
pBlock = StatBlock <$> braced (many pStatDecl)

pStatDecl :: Parser Token Stat
pStatDecl =  pStat
         <|> StatDecl <$> pDeclSemi

pStat :: Parser Token Stat
pStat =  StatExpr <$> pExpr1 <*  sSemi
     <|> StatIf     <$ symbol KeyIf     <*> parenthesised pExpr1 <*> pStat <*> optionalElse
     <|> StatWhile  <$ symbol KeyWhile  <*> parenthesised pExpr1 <*> pStat
     <|> StatReturn <$ symbol KeyReturn <*> pExpr1               <*  sSemi
     <|> pBlock
     where optionalElse = option (symbol KeyElse *> pStat) (StatBlock [])

pExpr1, pExpr2, pExpr3, pExpr4, pExpr5, pExpr6, pExpr7, pExpr8, pExprSimple :: Parser Token Expr
pExpr1 = chainr pExpr2 op1
pExpr2 = chainl pExpr3 op2
pExpr3 = chainl pExpr4 op3
pExpr4 = chainl pExpr5 op4
pExpr5 = chainl pExpr6 op5
pExpr6 = chainl pExpr7 op6
pExpr7 = chainl pExpr8 op7
pExpr8 = chainr pExprSimple op8
pExprSimple =  ExprConst <$> sConst
           <|> ExprVar   <$> sLowerId
           <|> ExprCall  <$> sLowerId <*> parenthesised (option (listOf pExpr1 (symbol Comma)) [])
           <|> parenthesised pExpr1

op1, op2, op3, op4, op5, op6, op7, op8 :: Parser Token (Expr -> Expr -> Expr)
op1 = opHelper <$> symbol (Operator "=")
op2 = opHelper <$> symbol (Operator "||")
op3 = opHelper <$> symbol (Operator "&&")
op4 = opHelper <$> symbol (Operator "^")
op5 = opHelper <$> symbol (Operator "==") <|> opHelper <$> symbol (Operator "!=")
op6 = opHelper <$> symbol (Operator "<") <|> opHelper <$> symbol (Operator ">") <|> opHelper <$> symbol (Operator "<=") <|> opHelper <$> symbol (Operator ">=")
op7 = opHelper <$> symbol (Operator "+") <|> opHelper <$> symbol (Operator "-")
op8 = opHelper <$> symbol (Operator "*") <|> opHelper <$> symbol (Operator "/") <|> opHelper <$> symbol (Operator "%")
opHelper (Operator x) = ExprOper x


pDecl :: Parser Token Decl
pDecl = Decl <$> pType <*> sLowerId

pDeclSemi :: Parser Token Decl
pDeclSemi = pDecl <* sSemi

pType :: Parser Token Type
pType =  TypePrim <$> sStdType
     <|> TypeObj  <$> sUpperId


-- The `Token` equivalents to some basic parser combinators
parenthesised, bracketed, braced :: Parser Token b -> Parser Token b
parenthesised p = pack (symbol POpen) p (symbol PClose) --(p)
bracketed     p = pack (symbol SOpen) p (symbol SClose) --[p]
braced        p = pack (symbol COpen) p (symbol CClose) --{p}
