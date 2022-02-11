module CSharpCode where

import Prelude hiding (LT, GT, EQ)
import qualified Data.Map as M

import CSharpLex
import CSharpGram
import CSharpAlgebra
import SSM

{-
  This file contains a starting point for the code generation which should handle very simple programs.
-}

-- The types that we generate for each datatype: Our type variables for the algebra
type C = Code                   -- Class
type M = Code                   -- Member
type S = Env -> (Code, Env)     -- Statement
type E = ValueOrAddress -> Env -> (Code, Env) -- Expression

type Env = M.Map String Int

codeAlgebra :: CSharpAlgebra C M S E
codeAlgebra =
    ( codeClass
    , codeMember
    , codeStatement
    , codeExpr
    )

codeClass :: String -> [M] -> C
codeClass c ms = [Bsr "main", HALT] ++ concat ms

codeMember = (fMembDecl, fMembMeth)
  where
    fMembDecl :: Decl -> M
    fMembDecl d = []

    fMembMeth :: Type -> String -> [Decl] -> S -> M
    fMembMeth t x ps s = 
      let
        la env' = (M.foldr max 0 env') + 1

        -- TODO: Translate into fold? this shit took way too long :cry:
        -- Hi TA, would be nice if you could leave a comment on how this
        -- could be written as a fold function :)
        f :: [Decl] -> Env -> Env
        f [] env'              = env'
        f ((Decl _ d):xs) env' = f xs (M.insert d (la env') env')

        env = f ps M.empty
        size = M.size env
        (s', _) = s $ env
      in
        [LABEL x] ++ [LINK size] ++ s' ++ [UNLINK] ++ [RET]

codeStatement = (fStatDecl, fStatExpr, fStatIf, fStatWhile, fStatReturn, fStatBlock)
  where
    fStatDecl :: Decl -> S
    fStatDecl (Decl _ d) env = 
      -- finds the maximum local address, then adds one to it so it will be unique.
      -- then saves it into the environment with the corresponding name
      let
        la = (M.foldr max 0 env) + 1
        env' =  M.insert d la env
      in
        ([], env')

    fStatExpr :: E -> S
    fStatExpr e env = 
      let
        (e', _) = e Value env
      in
        (e' ++ [pop], env)

    fStatIf :: E -> S -> S -> S
    fStatIf e s1 s2 env = 
      let
        (s1', _) = s1 env
        (s2', _) = s2 env
        (n1, n2) = (codeSize s1', codeSize s2')
        (c, _)   = e Value env
      in
        ((c ++ [BRF (n1 + 2)] ++ s1' ++ [BRA n2] ++ s2'), env)

    fStatWhile :: E -> S -> S
    fStatWhile e s1 env = 
      let
        (s1', _) = s1 env
        (c, _)   = e Value env
        (n, k)   = (codeSize s1', codeSize c)
      in
        ([BRA n] ++ s1' ++ c ++ [BRT (-(n + k + 2))], env)

    fStatReturn :: E -> S
    fStatReturn e env = 
      let
        (e', _) = e Value env
      in
        -- stores value in register
        (e' ++ [STR r3], env)

    fStatBlock :: [S] -> S
    fStatBlock ss env = 
      foldl f ([], env) ss
      where
        -- sends the updated environment to the next statements in a code block
        f (preCode, preEnv) s = 
          let 
            (postCode, postEnv) = s preEnv 
          in 
            (preCode ++ postCode, postEnv)

codeExpr = (fExprCon, fExprVar, fExprOp, fExprCall)
  where
    fExprCon :: Int -> E
    fExprCon n va env = ([LDC n], env)

    fExprVar :: String -> E
    fExprVar x va env = 
      let
        loc = env M.! x
      in
        case va of 
          Value    ->  ([LDL  loc], env)
          Address  ->  ([LDLA loc], env)

    fExprOp :: String -> E -> E -> E
    fExprOp "=" e1 e2 va env = 
      let
        (e2', _) = e2 Value env
        (e1', _) = e1 Value env
      in
        (e2' ++ [LDS 0] ++ e1' ++ [STA 0], env)
    fExprOp "&&" e1 e2 va env =  
      let
        (e1', _) = e1 Value env
        (e2', _) = e2 Value env
      in
        (e1' ++ e1' ++ [BRF (codeSize e2' + 1)] ++ e2' ++ [AND], env)
    fExprOp "||" e1 e2 va env = 
      let
        (e1', _) = e1 Value env
        (e2', _) = e2 Value env
      in  
        (e1' ++ e1' ++ [BRT (codeSize e2' + 1)] ++ e2' ++ [OR], env)
    fExprOp op  e1 e2 va env = 
      let
        (e1', _) = e1 Value env
        (e2', _) = e2 Value env
      in
        (e1' ++ e2' ++ [opCodes M.! op], env)
      where
        opCodes :: M.Map String Instr
        opCodes = M.fromList [ ("+", ADD), ("-",  SUB), ("*", MUL), ("/", DIV), ("%", MOD)
                             , ("<=", LE), (">=",  GE), ("<",  LT), (">",  GT), ("==", EQ)
                             , ("!=", NE), ("&&", AND), ("||", OR), ("^", XOR)
                             ]

    fExprCall :: String -> [E] -> E
    fExprCall "print" args va env = (concatMap (\x -> fst $ x Value env) args ++ [TRAP 0], env)
    fExprCall x args va       env = (concatMap (\x -> fst $ x Value env) args ++ [Bsr x] ++ [LDR R3], env)

-- | Whether we are computing the value of a variable, or a pointer to it
data ValueOrAddress = Value | Address
  deriving Show
