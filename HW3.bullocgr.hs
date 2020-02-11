-- CS 381 HW 3
-- Grace Bullock, Alexander Molotkov, Zeyad Shureihz

module HW3 where

import Data.List

-- 1 ------------------------------------------------------------------------------------

-- Num is already defined
type Macro = [Char]
type Var = [Char]

-- May have to deal with programs appending on to programs
type Prog = [Cmd]

data Mode = Down | Up
  deriving (Eq,Show)

data Expr = Var Var
          | Num Int
          | Add Expr Expr
  deriving (Eq,Show)

data Cmd = Pen Mode
         | Move Expr Expr
         | Define Macro [Var] Prog
         | Call Macro [Expr]
  deriving(Eq,Show)

addVars :: String -> String -> Expr
addVars s1 s2 = Add (Var s1) (Var s2)

-- 2 ------------------------------------------------------------------------------------
--
-- define line (x1, y1, x2, y2){
--  pen up; move (x1,y1);
--  pen down; move (x2,y2);
-- }
--

--Macro that defines a line
line = [ Define "line" ["x1", "y1", "x2", "y2"] 
       [ Pen Up, Move (Var "x1") (Var "y1"), Pen Down, Move (Var "x2") (Var "y2") ] ]

--Example of line call
--LineX = Call "line" [Var "x1", Var "y1", Var "x2", Var "y2"]

-- 3 ------------------------------------------------------------------------------------
--
-- define nix (x, y, w, h){
--  call line(x, y, x+w, y+h);
--  call line(x, y+h, x+w, y);
-- }
--

nix = [Define "nix" ["x", "y", "w", "h"]
      [Call "line" [Var "x", Var "y", addVars "x" "w", addVars "y" "h"],
      Call "line" [Var "x", addVars "y" "h", addVars "x" "w", Var "y"]]]

-- 4 ------------------------------------------------------------------------------------

steps :: Int -> Prog
steps i = if i > 0 then steps (i - 1) ++ 
         [Call "line" [Num i, Num i, Num(i-1), Num i],                 -- Horizontal line
          Call "line" [Num (i-1), Num i, Num (i-1), Num (i-1)] ] else [] -- Vertical line

-- 5 ------------------------------------------------------------------------------------

macros :: Prog -> [Macro]
macros []          = []
macros (cmd1:cmd2) = case cmd1 of 
  (Define s _ _) -> [s] ++ macros cmd2
  otherwise      -> []  ++ macros cmd2

-- 6 ------------------------------------------------------------------------------------

-- Replaces a character inside a string up until i characters from the end of the string
-- (I'm not happy about it either)
replUntil :: Char -> String -> Int -> String -> String
replUntil _ _ _ []          = []
replUntil delim s i (c1:c2) = 
    if   c1 == delim && (length c2) > i 
    then s ++ replUntil delim s i c2 
    else c1 : replUntil delim s i c2

prettyExpr :: Expr -> String
prettyExpr (Var v)     = v
prettyExpr (Num x)     = show x
prettyExpr (Add s1 s2) = prettyExpr s1 ++ "+" ++ prettyExpr s2 

prettyPen :: Cmd -> String
prettyPen (Pen Up)   = "pen up;\n"
prettyPen (Pen Down) = "pen down;\n"

prettyMove :: Cmd -> String
prettyMove (Move e1 e2) = 
    "move (" 
    ++ (prettyExpr e1) 
    ++ "," 
    ++ (prettyExpr e2) 
    ++ ");\n"

prettyDefine :: Cmd -> String
prettyDefine (Define name vars prog) = 
    "define " 
    ++ name 
    ++ " (" 
    ++ intercalate ", " vars 
    ++ "){\n    " 
    ++ replUntil '\n' "\n    " 3 (pretty prog) 
    ++ "}\n"

prettyCall :: Cmd -> String
prettyCall (Call name vars) = 
    "call " 
    ++ name 
    ++ "(" 
    ++ intercalate ", " (map prettyExpr vars) 
    ++ ");\n"

pretty :: Prog -> String
pretty []          = []
pretty (cmd1:cmd2) = case cmd1 of
    Pen _        -> prettyPen cmd1    ++ pretty cmd2
    Move _ _     -> prettyMove cmd1   ++ pretty cmd2
    Define _ _ _ -> prettyDefine cmd1 ++ pretty cmd2
    Call _ _     -> prettyCall cmd1   ++ pretty cmd2

-- 7 ------------------------------------------------------------------------------------

optE :: Expr -> Expr
optE (Num x)     = Num x
optE (Var s)     = Var s
optE (Add e1 e2) =  case e1 of
    Num x     -> case e2 of
        Num y     -> Num (x+y)    
        otherwise -> Add (optE e1) (optE e2)
    otherwise -> Add (optE e1) (optE e2)

-- 8 ------------------------------------------------------------------------------------

optP :: Prog -> Prog
optP []      = []
optP (c1:c2) = case c1 of
    Move e1 e2     -> (Move (optE e1) (optE e2)) : optP c2
    Call m exlist  -> (Call m (map optE exlist)) : optP c2
    Define mac v p -> (Define mac v (optP p)) : optP c3
    otherwise      -> c1 : optP c2