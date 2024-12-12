-- parser bug
-- typechecker bug
import Data.Char
import Data.Maybe (catMaybes)
-- 1. Datatype Definitions

type TVars = String
type  Vars = String

data Types = TVar TVars | Fun Types Types | Unit | List Types
           | Bool | Prod Types Types | Int
  deriving (Eq)
data Terms = Var Vars | App Terms Terms | Abs Vars Terms | UT
           | Nil | Cons Terms Terms | Fold Terms Terms Terms
           | TT | FF | IfThenElse Terms Terms Terms
           | Pair Terms Terms | Fst Terms | Snd Terms
           | Num Int | Add Terms Terms | Lte Terms Terms
           | IsPos Terms    -- Add IsPos
           | Sub Terms Terms -- Add subtraction
           | Y              -- Add Y-combinator
           | Mul Terms Terms
  deriving (Show,Eq)

data Token = VSym Vars | Backslash | Dot | LPar | RPar | USym
           | NilT | ConsT | FoldT 
           | TTT | FFF | IfT | ThenT | ElseT
           | PairT | FstT | SndT | Comma
           | NumT Int | PlusT | LteT
           | IsPosT | SubT | YT    -- Add new tokens
           | IfThenElseT           -- Add this token
           | PT Terms | Err String
           | MulT
  deriving (Show,Eq)

instance Show Types where
  show (TVar a) = a
  show Unit = "1"
  show (List a) = "[" ++ show a ++ "]"
  show (Bool) = "B"
  show (Int) = "Z" 
  show (Prod t1 t2) = "(" ++ show t1 ++ " * " ++ show t2 ++ ")"
  show (Fun t1@(Fun _ _) t2) = "(" ++ show t1 ++ ")" ++ " -> " ++ show t2
  show (Fun t1 t2) = show t1 ++ " -> " ++ show t2
-- 2. Lexer

lexer :: String -> [Token]
lexer "" = []
lexer ('(':')':xs) = USym : lexer xs
lexer ('[':']':xs) = NilT : lexer xs
lexer ('@':xs) = ConsT : lexer xs
lexer ('f':'o':'l':'d':xs) = FoldT : lexer xs
lexer ('.' :xs) = Dot  : lexer xs
lexer ('\\':xs) = Backslash : lexer xs
lexer ('(' :xs) = LPar : lexer xs
lexer (')' :xs) = RPar : lexer xs
lexer ('t':'t':xs) = TTT : lexer xs
lexer ('f':'f':xs) = FFF : lexer xs
lexer ('I':'f':'T':'h':'e':'n':'E':'l':'s':'e':xs) = IfThenElseT : lexer xs
lexer ('I':'s':'P':'o':'s':xs) = IsPosT : lexer xs
lexer ('i':'f':xs) = IfT : lexer xs
lexer ('t':'h':'e':'n':xs) = ThenT : lexer xs
lexer ('e':'l':'s':'e':xs) = ElseT : lexer xs
lexer ('p':'a':'i':'r':xs) = PairT : lexer xs
lexer ('f':'s':'t':xs) = FstT : lexer xs
lexer ('s':'n':'d':xs) = SndT : lexer xs
lexer ('Y':xs) = YT : lexer xs
lexer ('+':xs) = PlusT : lexer xs
lexer ('≤':xs) = LteT : lexer xs
lexer ('<':'=':xs) = LteT : lexer xs
lexer ('-':xs) = SubT : lexer xs
lexer ('*':xs) = MulT : lexer xs
lexer (',':xs) = Comma : lexer xs
lexer (x:xs) | x == '-' && isDigit (head xs) = 
  let (num,rest) = span isDigit xs
  in NumT (-(read num)) : lexer rest
lexer (x:xs) | isDigit x = 
  let (num,rest) = span isDigit (x:xs)
  in NumT (read num) : lexer rest
lexer (x : xs) | isSpace x = lexer xs
lexer (x : xs) | isAlpha x = VSym (x : takeWhile isAlphaNum xs)
                              : lexer (dropWhile isAlphaNum xs)
lexer xs = [Err (drop 10 xs)]

-- 3. Parser

{-
-- A basic prototype of the shift-reduce parser, mirroring the grammar exactly
-- while providing minimal support for parentheses and lexical errors.
-- This code is provided for comparison with the more advanced one below.
sr (VSym x        : s) q = sr (PT (Var x) : s) q
sr (PT t2 : PT t1 : s) q = sr (PT (App t1 t2) : s) q
sr (PT t : Dot : PT (Var x) : Backslash : s) q = sr (PT (Abs x t) : s) q
sr (RPar : PT t : LPar : s) q = sr (PT t : s) q
sr s (q:qs)      = sr (q:s) qs
sr (Err e : s) q = [Err e]
sr s []          = s
-}

-- A fine-tuned parser for lambda terms, with extra support for the following notations:
-- * A sequence of variables between lambda and dot are desugared into individual lambdas
-- * Parentheses around the body of the lambda are unnecessary;
--   the scope of variables bound before the dot extends as far out as possible.
sr :: [Token] -> [Token] -> [Token]
-- Unit
sr (USym : s) q = sr (PT UT : s) q
-- Lists
sr (NilT : s) q = sr (PT Nil : s) q
sr (PT t2 : ConsT : PT t1 : s) q = sr (PT (Cons t1 t2) : s) q
sr (PT t3 : PT t2 : PT t1 : FoldT : s) q = sr (PT (Fold t1 t2 t3) : s) q
-- Booleans
sr (TTT : s) q = sr (PT TT : s) q
sr (FFF : s) q = sr (PT FF : s) q
-- If-Then-Else
sr (RPar : PT e3 : Comma : PT e2 : Dot : VSym x : Backslash : Comma : PT e1 : LPar : IfThenElseT : s) q = 
  sr (PT (IfThenElse e1 (Abs x e2) e3) : s) q
sr (RPar : PT e3 : Comma : PT e2 : Comma : PT e1 : LPar : IfThenElseT : s) q = 
  sr (PT (IfThenElse e1 e2 e3) : s) q
-- If-Then-Else with conditions
sr (RPar : PT e3 : Comma : PT e2 : Comma : PT (Lte t1 t2) : LPar : IfThenElseT : s) q = 
  sr (PT (IfThenElse (Lte t1 t2) e2 e3) : s) q
sr (RPar : PT e3 : Comma : PT e2 : Comma : PT e1 : LPar : IfThenElseT : s) q = 
  sr (PT (IfThenElse e1 e2 e3) : s) q
-- Pairs
sr (RPar : PT t2 : PT t1 : LPar : s) q = sr (PT (Pair t1 t2) : s) q
sr (PT t : FstT : s) q = sr (PT (Fst t) : s) q
sr (PT t : SndT : s) q = sr (PT (Snd t) : s) q
-- Pairs with comma notation
sr (RPar : PT t2 : Comma : PT t1 : LPar : s) q = sr (PT (Pair t1 t2) : s) q
-- Numbers and Operations
sr (NumT n : s) q = sr (PT (Num n) : s) q
sr (PT t2 : PlusT : PT t1 : s) q = sr (PT (Add t1 t2) : s) q
sr (PT t2 : LteT : PT t1 : s) q = sr (PT (Lte t1 t2) : s) q
sr (PT t2 : SubT : PT t1 : s) q = sr (PT (Sub t1 t2) : s) q
sr (PT t2 : MulT : PT t1 : s) q = sr (PT (Mul t1 t2) : s) q

-- IsPos (move before parentheses rules)
sr (RPar : PT (Num n) : SubT : LPar : IsPosT : s) q = sr (PT (IsPos (Num (-n))) : s) q
sr (RPar : PT t : LPar : IsPosT : s) q = sr (PT (IsPos t) : s) q

-- Parentheses (general case)
sr (RPar : PT t : LPar : s) q = sr (PT t : s) q

-- Application
sr (PT t2 : PT t1 : s) q = sr (PT (App t1 t2) : s) q
-- Abstraction
sr st@(VSym x : Backslash : s) (q : qs) = sr (q : st) qs
sr (VSym y : VSym x : Backslash : s) q = 
  sr (VSym y : Backslash : Dot : VSym x : Backslash : s) q
sr (PT t : Dot : VSym x : Backslash : s) [] = 
  sr (PT (Abs x t) : s) []
sr (RPar : PT t : Dot : VSym x : Backslash : s) q = 
  sr (RPar : PT (Abs x t) : s) q
-- Variables
sr (VSym x : s) q = sr (PT (Var x) : s) q
-- Shift
sr s (q:qs) = sr (q:s) qs
-- Error
sr (Err s : st) q = [Err s]
-- Return
sr s [] = s

-- 4. Generating variable names

-- A list of all possible variables (infinite)
allVars :: [String]
allVars = tail vars where
  expand s = map (\c -> s ++ [c]) ['a'..'z']
  vars = "" : concat (map expand vars)

-- Function to generate fresh varaibles
-- Version 1.  Only works when allVars is finite.
-- freshVar :: [TVars] -> TVars
-- freshVar tvs =
--   let indexedVars = zip allVars [1..]
--       occurringVars = filter (\(x,i) -> elem x tvs) indexedVars
--       maxIndex = maximum (map snd occurringVars)
--    in allVars !! maxIndex

-- Version 2.  Works with infinite TVars
freshVar :: [String] -> String
freshVar tvs =
  let indexedVars = zip allVars [1..]
      indices = catMaybes (map (flip lookup indexedVars) tvs)
      maxIndex = maximum indices
   in allVars !! maxIndex

-- 5. Contexts and type constraints

-- A context is a lookup table associating varaibles to types
type Cxt = [(Vars,Types)]
-- A constraint is represented by a pair of types
type Constr = (Types,Types)
-- A substitution is a mapping from type variables to types
type TSub = [(TVars,Types)]

tsubst :: (TVars,Types) -> Types -> Types
tsubst (x,t) Unit = Unit
tsubst (a,t) (TVar b) = if a==b then t else TVar b
tsubst (a,t) (List l) = List (tsubst (a,t) l)
tsubst (a,t) (Fun t1 t2) = Fun (tsubst (a,t) t1) (tsubst (a,t) t2)
tsubst (a,t) (Prod t1 t2) = Prod (tsubst (a,t) t1) (tsubst (a,t) t2)
tsubst (a,t) Bool = Bool
tsubst (a,t) Int = Int
csubst :: (TVars,Types) -> Constr -> Constr
csubst s (lhs,rhs) = (tsubst s lhs , tsubst s rhs)

-- Helper functions extracting variables from types and contexts
getTVars :: Types -> [TVars]
getTVars (TVar a)    = [a]
getTVars (Unit)      = []
getTVars (List t)    = getTVars t
getTVars (Fun t1 t2) = getTVars t1 ++ getTVars t2
getTVars (Bool)      = []
getTVars (Int)       = []
getTVars (Prod t1 t2) = getTVars t1 ++ getTVars t2
getTVarsCxt :: Cxt -> [TVars]
getTVarsCxt []              = []
getTVarsCxt ((x,t) : gamma) = getTVars t ++ getTVarsCxt gamma
tvarsConstrs :: [Constr] -> [TVars]
tvarsConstrs []         = []
tvarsConstrs ((l,r):cs) = getTVars l ++ getTVars r ++ tvarsConstrs cs
-- 6. Type inference

-- The function to generate the constraints
genConstrs :: Cxt -> Terms -> Types -> [Constr]
genConstrs gamma (Var x)   a = case lookup x gamma of
             Just a' -> [(a',a)]
             Nothing -> error $ "Variable undeclared: " ++ x
genConstrs gamma (App s t) b =
  let a = freshVar (getTVars b ++ getTVarsCxt gamma)
      cs1 = genConstrs gamma s (Fun (TVar a) b)
      a' = freshVar (a : tvarsConstrs cs1)
      cs2 = genConstrs gamma t (TVar a')
   in (TVar a,TVar a') : cs1 ++ cs2
genConstrs gamma (Abs x t) a =
  let tvs = getTVars a ++ getTVarsCxt gamma
      a1 = freshVar tvs
      a2 = freshVar (a1 : tvs)
      cs = genConstrs ((x,TVar a1): gamma) t (TVar a2)
   in (a,Fun (TVar a1) (TVar a2)) : cs
genConstrs gamma (UT)      a = [(a,Unit)]
genConstrs gamma (Nil)     a = [(a,List (TVar b))]
  where b = freshVar (getTVars a ++ getTVarsCxt gamma)
genConstrs gamma (Cons h t) b =
  let a = freshVar (getTVars b ++ getTVarsCxt gamma)
      cs1 = genConstrs gamma h (TVar a)
      a' = freshVar (a : tvarsConstrs cs1)
      cs2 = genConstrs gamma t (TVar a')
   in (TVar a' , List (TVar a)) : (b,TVar a') : cs1 ++ cs2
genConstrs gamma (Fold s t u) b =
  let cs1 = genConstrs gamma t b
      cs1vars = tvarsConstrs cs1
      a = freshVar (getTVars b ++ getTVarsCxt gamma ++ cs1vars)
      cs2 = genConstrs gamma s (Fun (TVar a) (Fun b b))
      a' = freshVar (a : tvarsConstrs cs2)
      cs3 = genConstrs gamma u (TVar a')
   in (TVar a', List (TVar a)) : cs1 ++ cs2 ++ cs3
genConstrs gamma TT a = [(a,Bool)]
genConstrs gamma FF a = [(a,Bool)]
genConstrs gamma (IfThenElse c t e) a = 
  let cs1 = genConstrs gamma c Bool
      cs2 = genConstrs gamma t a
      cs3 = genConstrs gamma e a
  in cs1 ++ cs2 ++ cs3
genConstrs gamma (Pair s t) a =
  let a1 = freshVar (getTVars a ++ getTVarsCxt gamma)
      a2 = freshVar (a1 : getTVars a ++ getTVarsCxt gamma)
      cs1 = genConstrs gamma s (TVar a1)
      cs2 = genConstrs gamma t (TVar a2)
  in (a, Prod (TVar a1) (TVar a2)) : cs1 ++ cs2
genConstrs gamma (Fst t) a =
  let b = freshVar (getTVars a ++ getTVarsCxt gamma)
      cs = genConstrs gamma t (Prod a (TVar b))
  in cs
genConstrs gamma (Snd t) a =
  let b = freshVar (getTVars a ++ getTVarsCxt gamma)
      cs = genConstrs gamma t (Prod (TVar b) a)
  in cs
genConstrs gamma (Num _) a = [(a,Int)]
genConstrs gamma (Add s t) a = 
  [(a,Int)] ++ genConstrs gamma s Int ++ genConstrs gamma t Int
genConstrs gamma (Lte s t) a =
  [(a,Bool)] ++ genConstrs gamma s Int ++ genConstrs gamma t Int
genConstrs gamma (IsPos t) a = 
  [(a,Bool)] ++ genConstrs gamma t Int
genConstrs gamma (Sub s t) a = 
  [(a,Int)] ++ genConstrs gamma s Int ++ genConstrs gamma t Int
genConstrs gamma Y a =
  let a1 = freshVar (getTVars a)
  in [(a, Fun (Fun (TVar a1) (TVar a1)) (TVar a1))]
genConstrs gamma (Mul s t) a = 
  [(a,Int)] ++ genConstrs gamma s Int ++ genConstrs gamma t Int

-- Solve the list of constraints
unify :: [Constr] -> TSub
unify [] = []
unify ((lhs,rhs):cs) | lhs == rhs = unify cs
unify ((TVar a,rhs):cs)
  | elem a (getTVars rhs) = error $ "Circular constraint: " ++ a ++ " = " ++ show rhs
  | otherwise             = (a,rhs) : unify (map (csubst (a,rhs)) cs)
unify ((lhs,TVar a):cs) = unify ((TVar a,lhs) : cs)
unify ((Fun s1 s2,Fun t1 t2) : cs) = unify ((s1,t1) : (s2,t2) : cs)
unify ((List a,List b) : cs) = unify ((a,b) : cs)
unify ((Prod s1 s2,Prod t1 t2) : cs) = unify ((s1,t1) : (s2,t2) : cs)
unify ((lhs,rhs):cs) = error $ "Type error: " ++ show lhs ++ " = " ++ show rhs

-- Applying a list of type substitutions to one type
tsubstList :: TSub -> Types -> Types
tsubstList []          s = s
tsubstList ((a,t):sub) s = tsubstList sub (tsubst (a,t) s)

-- chaining the lexer, parser, constraint generator, and unifier
infer :: String -> Types
infer s = case sr [] (lexer s) of
  [PT t] -> tsubstList (unify (genConstrs [] t (TVar "a"))) (TVar "a")
  s      -> error $ "Parse error!\n" ++ show s

-- 7. Reduction

fv :: Terms -> [Vars]
fv (Var x) = [x]
fv (App s t) = fv s ++ fv t
fv (Abs x t) = filter (/= x) (fv t)
fv (UT) = []
fv (Nil) = []
fv (Cons h t) = fv h ++ fv t
fv (Fold s t u) = fv s ++ fv t ++ fv u

subst :: (Vars,Terms) -> Terms -> Terms
subst (x,t) (Var y) = if x==y then t else Var y
subst xt (App u v) = App (subst xt u) (subst xt v)
subst (x,t) (Abs y r)
  | x==y = (Abs y r)
  | not (elem y (fv t)) = Abs y (subst (x,t) r)
  | otherwise = 
      let z = freshVar (x : y : fv t ++ fv r)
          r' = subst (y , Var z) r
       in Abs z (subst (x,t) r')
subst xt (UT) = UT
subst xt (Nil) = Nil
subst xt (Cons s t) = Cons (subst xt s) (subst xt t)
subst xt (Fold s t u) = Fold (subst xt s) (subst xt t) (subst xt u)
subst xt TT = TT
subst xt FF = FF
subst xt (IfThenElse c t e) = IfThenElse (subst xt c) (subst xt t) (subst xt e)
subst xt (Pair s t) = Pair (subst xt s) (subst xt t)
subst xt (Fst t) = Fst (subst xt t)
subst xt (Snd t) = Snd (subst xt t)
subst xt (Num n) = Num n
subst xt (Add s t) = Add (subst xt s) (subst xt t)
subst xt (Lte s t) = Lte (subst xt s) (subst xt t)

-- Root reduction (->0)
redRoot :: Terms -> Maybe Terms
redRoot (App (Abs x s) t) = Just $ subst (x,t) s                    -- (1)
redRoot (Fst (Pair s t)) = Just s                                   -- (2)
redRoot (Snd (Pair s t)) = Just t                                   -- (3)
redRoot (IfThenElse TT t _) = Just t                               -- (4)
redRoot (IfThenElse FF _ e) = Just e                               -- (5)
redRoot (Add (Num n1) (Num n2)) = Just $ Num (n1 + n2)            -- (6)
redRoot (IsPos (Num n)) = Just $ if n > 0 then TT else FF         -- (7)
redRoot (App Y t) = Just $ App t (App Y t)                        -- (8)
redRoot _ = Nothing

-- Parallel reduction (->)
redPar :: Terms -> Terms
redPar t = case redRoot t of
  Just t' -> t'
  Nothing -> case t of
    -- Variables and constants reduce to themselves
    Var x -> Var x
    Num n -> Num n
    TT -> TT
    FF -> FF
    Y -> Y
    
    -- Congruence rules
    App s t -> App (redPar s) (redPar t)
    Abs x r -> Abs x (redPar r)
    Pair s t -> Pair (redPar s) (redPar t)
    Fst t -> Fst (redPar t)
    Snd t -> Snd (redPar t)
    IfThenElse c t e -> IfThenElse (redPar c) (redPar t) (redPar e)
    Add s t -> Add (redPar s) (redPar t)
    Sub s t -> Sub (redPar s) (redPar t)
    Mul s t -> Mul (redPar s) (redPar t)
    IsPos t -> IsPos (redPar t)
    _ -> t

-- Pretty neat little test function
showResult :: String -> IO ()
showResult input = do
  putStrLn $ "\nTest: " ++ input
  case sr [] (lexer input) of 
    [PT term] -> do
      putStrLn $ "Parsed term: " ++ show term
      putStrLn $ "Type: " ++ show (infer input)
      putStrLn $ "Reduced: " ++ show (redPar term)
    err -> putStrLn $ "Parse error: " ++ show err

-- List of test cases
tests :: [String]
tests = [
  "\\x.\\y.x (y x)",
  "\\x.\\y.(x y y)",
  "\\x.\\y.(y (y x))",
  "\\x.\\y.x (y x)",
  "\\x.\\y.\\z.((x z), (y z))",
  "\\p.\\f.f (snd p) (fst p)",
  "(tt, \\x.x(ff))",
  "IfThenElse (tt, \\x.x, \\y.ff)",
  "\\x.\\y.y(IfThenElse(x, tt, y x))",
  "\\x.IfThenElse(x <= 0, 0, x)",
  "\\x.\\y.x + y",
  "1",
  "\\x.x + 1",
  "\\x.\\y.IfThenElse(x <= y, x, y)",
  "\\x.\\y.x(fst y) + snd y",
  "IsPos(1)",
  "IsPos(-1)",
  "\\x.IsPos(x)",
  "\\x.\\y.x - y"
  ]

-- Main function to run all tests
main :: IO ()
main = mapM_ showResult tests
