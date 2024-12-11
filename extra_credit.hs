-- parser bug
-- typechecker bug
import Data.Char
import Data.Maybe (catMaybes)
-- 1. Datatype Definitions

type TVars = String
type  Vars = String

data Types = TVar TVars | Fun Types Types | Unit | List Types
          | Prod Types Types | Bool | Ints  -- Added Ints type
          deriving (Eq)
data Terms = Var Vars | App Terms Terms | Abs Vars Terms | UT
           | Nil | Cons Terms Terms | Fold Terms Terms Terms
           | Pair Terms Terms      -- New: Pairs
           | Fst Terms            -- New: First projection
           | Snd Terms            -- New: Second projection
           | TT | FF              -- New: Boolean constants
           | IfThenElse Terms Terms Terms  -- New: If-then-else
           | IsPos Terms          -- New: IsPositive test
           | Y                    -- New: Y combinator
           | Num Integer          -- New: Explicit number constructor
           deriving (Show,Eq)

data Token = VSym Vars | Backslash | Dot | LPar | RPar | USym
           | NilT | ConsT | FoldT -- New tokens for working with lists
           | PT Terms | Err String
           | EqSym | LoadT | QuitT | TypeT
           | Comma  -- Add comma token
  deriving (Show,Eq)

instance Show Types where
  show (TVar a) = a
  show (Unit)   = "1"
  show (List a) = "[" ++ show a ++ "]"
  -- show (Fun t1 t2) = "(" ++ show t1 ++ " -> " ++ show t2 ++ ")"
  show (Fun t1@(Fun _ _) t2) = "(" ++ show t1 ++ ")" ++ " -> " ++ show t2
  show (Fun t1 t2) = show t1 ++ " -> " ++ show t2
  show (Prod a b) = "(" ++ show a ++ " × " ++ show b ++ ")"
  show (Bool) = "Bool"
  show (Ints) = "Z"
-- 2. Lexer

lexer :: String -> [Token]
lexer "" = []
lexer ('=':xs) = EqSym : lexer xs
lexer (':':'q':xs) = QuitT : lexer xs
lexer (':':'l':xs) = LoadT : lexer xs
lexer (':':'t':xs) = TypeT : lexer xs
lexer ('(':')':xs) = USym : lexer xs
lexer ('[':']':xs) = NilT : lexer xs
lexer ('@':xs) = ConsT : lexer xs
lexer ('f':'o':'l':'d':xs) = FoldT : lexer xs
lexer ('.' :xs) = Dot  : lexer xs
lexer ('\\':xs) = Backslash : lexer xs
lexer ('(' :xs) = LPar : lexer xs
lexer (')' :xs) = RPar : lexer xs
lexer (',' :xs) = Comma : lexer xs  -- Add comma handling
lexer (x : xs) | isSpace x = lexer xs
lexer (x : xs) | isAlpha x = VSym (x : takeWhile isAlphaNum xs)
                              : lexer (dropWhile isAlphaNum xs)
-- alternative variable handler
-- lexer (x:xs) | isAlpha x = VSym var : lexer rest
--     where (var,rest) = span isAlphaNum (x:xs)
lexer xs = [Err (take 10 xs)]

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
-- Add these rules for pairs (before other rules)
sr (RPar : PT t2 : Comma : PT t1 : LPar : s) q = sr (PT (Pair t1 t2) : s) q
-- Unit
sr (USym : s) q = sr (PT UT : s) q
-- Lists
sr (NilT                          : s) q = sr (PT Nil : s) q
sr (PT t2 : ConsT : PT t1         : s) q = sr (PT (Cons t1 t2) : s) q
sr (PT t3 : PT t2 : PT t1 : FoldT : s) q = sr (PT (Fold t1 t2 t3) : s) q
-- Parentheses
sr (RPar : PT t : LPar : s) q = sr (PT t : s) q
-- Application
sr st@(PT t2 : PT t1 : FoldT : s) (q : qs) = sr (q:st) qs
sr (PT t2 : PT t1      : s) q = sr (PT (App t1 t2) : s) q
-- Abstraction
sr st@(VSym x : Backslash : s) (q : qs) = sr (q : st) qs -- Blocks Var rule after lambda
sr (VSym y : VSym x : Backslash : s) q    -- After a lambda, a sequence of vars becomes
  = sr (VSym y : Backslash : Dot : VSym x : Backslash : s) q  -- a sequence of lambdas
sr (PT t : Dot : VSym x : Backslash : s) []  -- The scope of abstracted variables
  = sr (PT (Abs x t) : s) []                 -- extends as far as possible:
sr (RPar : PT t : Dot : VSym x : Backslash : s) q  -- either to the end of the term
  = sr (RPar : PT (Abs x t) : s) q                 -- or until a closing parens
-- Variables
sr (VSym x : s)             q = sr (PT (Var x) : s) q
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
tsubst (x,t) (Unit)      = Unit
tsubst (a,t) (TVar b)    = if a==b then t else TVar b
tsubst (a,t) (List l)    = List (tsubst (a,t) l)
tsubst (a,t) (Fun t1 t2) = Fun (tsubst (a,t) t1) (tsubst (a,t) t2)
tsubst (a,t) (Prod t1 t2) = Prod (tsubst (a,t) t1) (tsubst (a,t) t2)
tsubst (a,t) Bool = Bool
tsubst (a,t) Ints = Ints
csubst :: (TVars,Types) -> Constr -> Constr
csubst s (lhs,rhs) = (tsubst s lhs , tsubst s rhs)

-- Helper functions extracting variables from types and contexts
getTVars :: Types -> [TVars]
getTVars (TVar a)    = [a]
getTVars (Unit)      = []
getTVars (List t)    = getTVars t
getTVars (Fun t1 t2) = getTVars t1 ++ getTVars t2
getTVars (Prod t1 t2) = getTVars t1 ++ getTVars t2
getTVars Bool = []
getTVars Ints = []
getTVarsCxt :: Cxt -> [TVars]
getTVarsCxt []              = []
getTVarsCxt ((x,t) : gamma) = getTVars t ++ getTVarsCxt gamma
tvarsConstrs :: [Constr] -> [TVars]
tvarsConstrs []         = []
tvarsConstrs ((l,r):cs) = getTVars l ++ getTVars r ++ tvarsConstrs cs
-- 6. Type inference

-- The function to generate the constraints
genConstrs :: Cxt -> Terms -> Types -> [Constr]
genConstrs gamma (Var "fst") a = 
    let b = freshVar (getTVars a ++ getTVarsCxt gamma)
        c = freshVar (b : getTVars a ++ getTVarsCxt gamma)
    in [(a, Fun (Prod (TVar b) (TVar c)) (TVar b))]

genConstrs gamma (Var "snd") a = 
    let b = freshVar (getTVars a ++ getTVarsCxt gamma)
        c = freshVar (b : getTVars a ++ getTVarsCxt gamma)
    in [(a, Fun (Prod (TVar b) (TVar c)) (TVar c))]

genConstrs gamma (Var x) a = case lookup x gamma of
    Just a' -> [(a',a)]
    Nothing -> error $ "Variable undeclared: " ++ x
genConstrs gamma (App s t) b =
    let a = freshVar (getTVars b ++ getTVarsCxt gamma)
        cs1 = genConstrs gamma s (Fun (TVar a) b)
        cs2 = genConstrs gamma t (TVar a)
    in cs1 ++ cs2
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

genConstrs gamma TT Bool = []
genConstrs gamma FF Bool = []
genConstrs gamma (IfThenElse c s t) a =
    let cs1 = genConstrs gamma c Bool
        cs2 = genConstrs gamma s a
        cs3 = genConstrs gamma t a
    in cs1 ++ cs2 ++ cs3
genConstrs gamma (IsPos t) Bool =
    genConstrs gamma t Ints
genConstrs gamma Y (Fun (Fun a b') b) 
    | b == b'   = []  -- Y has type ((a -> b) -> (a -> b)) -> (a -> b)
    | otherwise = error "Invalid type for Y combinator"
genConstrs gamma (Num _) Ints = []
genConstrs gamma (UT) a = [(a,Unit)]
genConstrs gamma (Nil) a = [(a,List (TVar b))]
  where b = freshVar (getTVars a ++ getTVarsCxt gamma)
genConstrs gamma (Cons h t) b =
  let a = freshVar (getTVars b ++ getTVarsCxt gamma)
      cs1 = genConstrs gamma h (TVar a)
      a' = freshVar (a : tvarsConstrs cs1)
      cs2 = genConstrs gamma t (TVar a')
   in (TVar a', List (TVar a)) : (b,TVar a') : cs1 ++ cs2
genConstrs gamma (Fold s t u) b =
  let cs1 = genConstrs gamma t b
      cs1vars = tvarsConstrs cs1
      a = freshVar (getTVars b ++ getTVarsCxt gamma ++ cs1vars)
      cs2 = genConstrs gamma s (Fun (TVar a) (Fun b b))
      a' = freshVar (a : tvarsConstrs cs2)
      cs3 = genConstrs gamma u (TVar a')
   in (TVar a', List (TVar a)) : cs1 ++ cs2 ++ cs3
genConstrs gamma (Abs x t) a =
  let tvs = getTVars a ++ getTVarsCxt gamma
      a1 = freshVar tvs
      a2 = freshVar (a1 : tvs)
      cs = genConstrs ((x,TVar a1): gamma) t (TVar a2)
   in (a,Fun (TVar a1) (TVar a2)) : cs

-- Solve the list of constraints
unify :: [Constr] -> Either String TSub
unify [] = Right []
unify ((lhs,rhs):cs) | lhs == rhs = unify cs
unify ((TVar a,rhs):cs)
  | elem a (getTVars rhs) = Left $ "Circular constraint: " ++ a ++ " = " ++ show rhs
  | otherwise = case unify (map (csubst (a,rhs)) cs) of
      Right sub -> Right $ (a,rhs) : sub
      Left err -> Left err
unify ((lhs,TVar a):cs) = unify ((TVar a,lhs) : cs)
unify ((Fun s1 s2,Fun t1 t2) : cs) = unify ((s1,t1) : (s2,t2) : cs)
unify ((Prod s1 s2,Prod t1 t2) : cs) = unify ((s1,t1) : (s2,t2) : cs)
unify ((List a,List b) : cs) = unify ((a,b) : cs)
unify ((lhs,rhs):cs) = Left $ "Type error: cannot unify " ++ show lhs ++ " with " ++ show rhs

-- Applying a list of type substitutions to one type
tsubstList :: TSub -> Types -> Types
tsubstList []          s = s
tsubstList ((a,t):sub) s = tsubstList sub (tsubst (a,t) s)

-- chaining the lexer, parser, constraint generator, and unifier
infer :: String -> Either String Types
infer s = case sr [] (lexer s) of
    [PT t] -> case unify (genConstrs [] t (TVar "a")) of
        Left err -> Left $ "Type error: " ++ err
        Right sub -> Right $ tsubstList sub (TVar "a")
    s -> Left $ "Parse error!\n" ++ show s

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
  | otherwise =  -- If the abstracted variable occurs in t, we must rename
      let z = freshVar (x : y : fv t ++ fv r)
          r' = subst (y , Var z) r -- r' is renaming y to z in the body of Abs
       in Abs z (subst (x,t) r')
subst xt (UT) = UT
subst xt (Nil) = Nil
subst xt (Cons s t)   = Cons (subst xt s) (subst xt t)
subst xt (Fold s t u) = Fold (subst xt s) (subst xt t) (subst xt u)

-- Root reduction (->0)
red0 :: Terms -> Maybe Terms
red0 (App (Abs x s) t) = Just $ subst (x,t) s
red0 (Fst (Pair s t)) = Just s
red0 (Snd (Pair s t)) = Just t
red0 (IfThenElse TT s t) = Just s
red0 (IfThenElse FF s t) = Just t
red0 (IsPos (Num n)) | n > 0 = Just TT
                     | otherwise = Just FF
red0 (App Y t) = Just $ App t (App Y t)
red0 _ = Nothing

-- Parallel reduction (->)
red :: Terms -> Terms
red t = case red0 t of
    Just t' -> t'
    Nothing -> case t of
        -- Congruence rules
        App s t -> App (red s) (red t)
        Abs x r -> Abs x (red r)
        Pair s t -> Pair (red s) (red t)
        Fst s -> Fst (red s)
        Snd s -> Snd (red s)
        IfThenElse c s t -> IfThenElse (red c) (red s) (red t)
        IsPos s -> IsPos (red s)
        -- Base cases (reflexivity)
        _ -> t

nf :: Terms -> Terms
nf t = if t == t' then t else nf t' where t' = red t

-- 8. Examples

ex0 :: String
ex0 = "(\\x.x(\\y.y x))(\\z .z)(\\y.y y)"

ex1 :: String
ex1 = "\\x.\\y.(x y y)"

ex2 :: String
ex2 = "\\x.\\y.(y (y x))"

ex3 :: String
ex3 = "\\x.\\y.x (y x)"


-- 9. Front end

type Env = [(Vars,Terms)]

substList :: Env -> Terms -> Terms
substList []     t = t
substList (s:sl) t = subst s (substList sl t)

parseSub :: [Token] -> (Vars,Terms)
parseSub (VSym x : EqSym : ts) = (x , parseTerm ts)
parseSub ts = error $ "Error in parseSub: " ++ show ts

parseTerm :: [Token] -> Terms
parseTerm ts = case sr [] ts of
  [PT t] -> t
  st     -> error ("Parse error: " ++ show st)

checkTerm :: Terms -> Either String Types
checkTerm t = 
    let initialType = TVar "a"
        constraints = genConstrs [] t initialType
    in case constraints of
        [] -> Right initialType
        cs -> case unify cs of
            Left err -> Left $ "Type error: " ++ err
            Right sub -> Right $ tsubstList sub initialType

-- Parse a term from string with error handling
parseTermString :: String -> Either Terms String 
parseTermString s = case sr [] (lexer s) of
    [PT t] -> Left t
    stack -> Right $ "Parse error: " ++ show stack

-- Main function that combines file processing with REPL
main :: IO ()
main = do
    putStrLn "PCF Interpreter"
    putStrLn "Type :q to quit, :l to load file, :t for type"
    repl []

repl :: Env -> IO ()
repl env = do
    inp <- getLine
    case lexer inp of
        [QuitT] -> putStrLn "Bye!" >> return ()
        [LoadT] -> do
            putStr "Enter file to load: "
            filename <- getLine
            content <- readFile filename
            case parseTermString content of
                Left term -> do
                    putStrLn $ "Parsed term: " ++ show term
                    case checkTerm term of
                        Right typ -> do
                            putStrLn $ "Type: " ++ show typ
                            let reduced = nf term
                            putStrLn $ "Reduced form: " ++ show reduced
                        Left err -> putStrLn err
                    repl env
                Right err -> do
                    putStrLn $ "Parse error: " ++ err
                    repl env
        (TypeT : ts) -> do
            let t = substList env (parseTerm ts)
            case checkTerm t of
                Right typ -> putStrLn (show typ)
                Left err -> putStrLn err
            repl env
        ts -> do
            let t = substList env (parseTerm ts)
            putStrLn (show (nf t))
            repl env
-- the end