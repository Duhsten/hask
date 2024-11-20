type Vars = String          -- Variables
type Value = Integer        -- Values
type Env = [(Vars,Value)]
data AExpr = Var Vars | Num Value -- Arithmetic expressions
            | Add AExpr AExpr | Sub AExpr AExpr | Mul AExpr AExpr
            | Div AExpr AExpr | Mod AExpr AExpr | Exp AExpr AExpr
    deriving (Eq,Show)

data BExpr = Const Bool -- Boolean expressions
            | And BExpr BExpr | Or BExpr BExpr | Not BExpr
            | Eq AExpr AExpr | Lt AExpr AExpr | Lte AExpr AExpr
            | Neq AExpr AExpr | Gt AExpr AExpr | Gte AExpr AExpr
    deriving (Eq,Show)

data Instr = Assign Vars AExpr -- assignment
            | IfThen BExpr Instr -- conditional
            | IfThenElse BExpr Instr Instr -- another conditional
            | While BExpr Instr -- looping construct
            | Do [Instr] -- a block of several instructions
            | Nop -- the "do nothing" instruction
            | Return AExpr -- the final value to return
    deriving (Eq,Show)


data Keywords = IfK | ThenK | ElseK | WhileK | NopK | ReturnK
    deriving (Eq,Show)

data BOps = AddOp | SubOp | MulOp | DivOp | ModOp | ExpOp
            | AndOp | OrOp | EqOp | NeqOp
            | LtOp | LteOp | GtOp | GteOp
    deriving (Eq,Show)

data Token = VSym String | CSym Integer | BSym Bool
            | LPar | RPar | LBra | RBra | Semi
            | BOp BOps | NotOp | AssignOp
            | Keyword Keywords
            | Err String
            | PA AExpr | PB BExpr | PI Instr | Block [Instr]
            | Comma
    deriving (Eq,Show)



isDigit :: Char -> Bool
isDigit x = elem x ['0'..'9']

isAlpha :: Char -> Bool
isAlpha x = elem x $ ['A'..'Z'] ++ ['a'..'z']

lexer :: String -> [Token]
-- keywords first
lexer xs | take 4 xs == "else" = Keyword ElseK : lexer (drop 4 xs)
lexer xs | take 2 xs == "if"   = Keyword IfK : lexer (drop 2 xs)
lexer xs | take 4 xs == "then" = Keyword ThenK : lexer (drop 4 xs)
lexer xs | take 5 xs == "while" = Keyword WhileK : lexer (drop 5 xs)
lexer xs | take 6 xs == "return" = Keyword ReturnK : lexer (drop 6 xs)

-- boolean constants
lexer xs | take 4 xs == "true"  = BSym True : lexer (drop 4 xs)
lexer xs | take 5 xs == "false" = BSym False : lexer (drop 5 xs)

-- integer constants
lexer (x:xs) | isDigit x =
  let (y,z) = span isDigit (x:xs)
   in CSym (read y) : lexer z

-- binary operators
lexer (':':'=':xs) = AssignOp : lexer xs
lexer ('=':'=':xs) = BOp EqOp : lexer xs
lexer ('=':xs) = AssignOp : lexer xs
lexer ('+':xs) = BOp AddOp : lexer xs
lexer ('-':xs) = BOp SubOp : lexer xs
lexer ('*':xs) = BOp MulOp : lexer xs
lexer ('/':xs) = BOp DivOp : lexer xs
lexer ('%':xs) = BOp ModOp : lexer xs
lexer ('^':xs) = BOp ExpOp : lexer xs
lexer ('|':'|':xs) = BOp OrOp : lexer xs
lexer ('&':'&':xs) = BOp AndOp : lexer xs
lexer ('!':'=':xs) = BOp NeqOp : lexer xs
lexer ('>':'=':xs) = BOp GteOp : lexer xs
lexer ('>':xs) = BOp GtOp : lexer xs
lexer ('<':'=':xs) = BOp LteOp : lexer xs
lexer ('<':xs) = BOp LtOp : lexer xs
lexer ('!':xs) = NotOp : lexer xs

-- punctuations
lexer ('{':xs) = LBra : lexer xs
lexer ('}':xs) = RBra : lexer xs
lexer ('(':xs) = LPar : lexer xs
lexer (')':xs) = RPar : lexer xs
lexer (',':xs) = Comma : lexer xs
lexer (';':xs) = Semi : lexer xs

-- variables
lexer (x:xs) | isAlpha x =
  let (y,z) = span isAlpha (x:xs)
   in VSym y : lexer z

-- whitespace
lexer (x:xs) | elem x " \t\n" = lexer xs
lexer "" = []
lexer xs = [Err (take 10 xs)]


precedence :: BOps -> Int
precedence op = case op of
    ExpOp -> 4
    MulOp -> 3
    DivOp -> 3
    ModOp -> 3
    AddOp -> 2
    SubOp -> 2
    EqOp  -> 1
    NeqOp -> 1
    LtOp  -> 1
    LteOp -> 1
    GtOp  -> 1
    GteOp -> 1
    AndOp -> 0
    OrOp  -> 0




-- Helper for logical operations
handleLogicOp :: BOps -> BExpr -> BExpr -> BExpr
handleLogicOp op a b = case op of
    AndOp -> And a b
    OrOp  -> Or a b
    _     -> error "Invalid logical operation"


-- Helper function to lookup variables in environment
lookupVar :: Vars -> Env -> Value
lookupVar v env = case lookup v env of
    Just val -> val
    Nothing -> error $ "Variable " ++ v ++ " not found"

-- Evaluate arithmetic expressions
evala :: Env -> AExpr -> Value
evala env (Num n) = n
evala env (Var v) = lookupVar v env
evala env (Add e1 e2) = evala env e1 + evala env e2
evala env (Sub e1 e2) = evala env e1 - evala env e2
evala env (Mul e1 e2) = evala env e1 * evala env e2
evala env (Div e1 e2) = evala env e1 `div` evala env e2
evala env (Mod e1 e2) = evala env e1 `mod` evala env e2
evala env (Exp e1 e2) = evala env e1 ^ evala env e2

-- Evaluate boolean expressions
evalb :: Env -> BExpr -> Bool
evalb env (Const b) = b
evalb env (And b1 b2) = evalb env b1 && evalb env b2
evalb env (Or b1 b2) = evalb env b1 || evalb env b2
evalb env (Not b) = not (evalb env b)
evalb env (Eq e1 e2) = evala env e1 == evala env e2
evalb env (Lt e1 e2) = evala env e1 < evala env e2
evalb env (Lte e1 e2) = evala env e1 <= evala env e2
evalb env (Neq e1 e2) = evala env e1 /= evala env e2
evalb env (Gt e1 e2) = evala env e1 > evala env e2
evalb env (Gte e1 e2) = evala env e1 >= evala env e2

-- Helper function to update variables in the environment
updateEnv :: Vars -> Value -> Env -> Env
updateEnv var val [] = [(var, val)]
updateEnv var val ((v, val'):env)
    | v == var  = (v, val) : env
    | otherwise = (v, val') : updateEnv var val env


-- Execute a single instruction
exec :: Instr -> Env -> Env
exec (Assign var expr) env = updateEnv var (evala env expr) env
exec (IfThen cond instr) env = 
    if evalb env cond 
    then exec instr env 
    else env
exec (IfThenElse cond instr1 instr2) env = 
    if evalb env cond 
    then exec instr1 env 
    else exec instr2 env
exec (While cond instr) env = 
    if evalb env cond 
    then exec (While cond instr) (exec instr env)
    else env
exec (Do instrs) env = execList instrs env
exec Nop env = env
exec (Return expr) env = [("", evala env expr)]

-- Execute a list of instructions
execList :: [Instr] -> Env -> Env
execList [] env = env
execList (i:is) env = execList is (exec i env)

prec :: BOps -> Int
prec AddOp = 10

-- Helper functions
isBOp :: Token -> Bool
isBOp (BOp _) = True
isBOp _ = False

getOp :: Token -> BOps
getOp (BOp op) = op
getOp _ = error "Expected a binary operator"

nextPrecedence :: [Token] -> Int
nextPrecedence (t:_) | isBOp t = precedence (getOp t)
nextPrecedence _ = -1  -- Lowest precedence

-- Shift-reduce parser with operator precedence
sr :: [Token] -> [Token] -> [Token]
sr stack ts =
    case reduce stack ts of
        Just newStack -> sr newStack ts
        Nothing ->
            case ts of
                (t:ts') -> sr (t:stack) ts'
                []      -> stack

-- Updated reduce function
reduce :: [Token] -> [Token] -> Maybe [Token]
-- Base cases for reducing variables and constants
reduce (VSym v:stack) _ = Just (PA (Var v):stack)
reduce (CSym n:stack) _ = Just (PA (Num n):stack)
reduce (BSym b:stack) _ = Just (PB (Const b):stack)

-- Reduce arithmetic expressions
reduce (PA y:BOp op1:PA x:stack) ts
    | nextPrecedence ts > precedence op1 = Nothing  -- Shift
    | op1 `elem` [AddOp, SubOp, MulOp, DivOp, ModOp, ExpOp] =
        Just (PA (handleArithOp op1 x y):stack)
    | op1 `elem` [EqOp, NeqOp, LtOp, LteOp, GtOp, GteOp] =
        Just (PB (handleBoolOp op1 x y):stack)
    | otherwise = Nothing

-- Reduce boolean expressions
reduce (PB y:BOp op1:PB x:stack) ts
    | nextPrecedence ts > precedence op1 = Nothing  -- Shift
    | op1 `elem` [AndOp, OrOp] =
        Just (PB (handleLogicOp op1 x y):stack)
    | otherwise = Nothing

-- Reduce Not expressions
reduce (PB b : NotOp : stack) _ = Just (PB (Not b) : stack)

-- Reduce assignment
reduce (Semi:PA e:AssignOp:PA (Var v):stack) _ = Just (PI (Assign v e):stack)

-- Reduce return statement
reduce (Semi:PA e:Keyword ReturnK:stack) _ = Just (PI (Return e):stack)

-- Reduce IfThen
reduce (PI instr : Keyword ThenK : PB cond : Keyword IfK : stack) _ =
    Just (PI (IfThen cond instr) : stack)

-- Reduce IfThenElse
reduce (PI elseInstr : Keyword ElseK : PI thenInstr : Keyword ThenK : PB cond : Keyword IfK : stack) _ =
    Just (PI (IfThenElse cond thenInstr elseInstr) : stack)

-- Reduce While with body
reduce (PI instr : PB cond : Keyword WhileK : stack) _ =
    Just (PI (While cond instr) : stack)

-- Do not reduce While without a body
reduce (PB _ : Keyword WhileK : _) _ = Nothing

-- Reduce expressions inside parentheses
reduce (RPar : PA e : LPar : stack) _ = Just (PA e : stack)
reduce (RPar : PB b : LPar : stack) _ = Just (PB b : stack)

-- Reduce code blocks into Do
reduce (RBra : tokens) ts =
    let (blockTokens, rest) = span (/= LBra) tokens
        instrs = parseInstructions (reverse blockTokens)
    in case rest of
        (LBra : stack) -> Just (PI (Do instrs) : stack)
        _              -> Nothing

reduce _ _ = Nothing  -- No reduction possible


-- Helper for arithmetic operations
handleArithOp :: BOps -> AExpr -> AExpr -> AExpr
handleArithOp AddOp a b = Add a b
handleArithOp SubOp a b = Sub a b
handleArithOp MulOp a b = Mul a b
handleArithOp DivOp a b = Div a b
handleArithOp ModOp a b = Mod a b
handleArithOp ExpOp a b = Exp a b
handleArithOp _ _ _ = error "Invalid arithmetic operation"

-- Helper for boolean operations
handleBoolOp :: BOps -> AExpr -> AExpr -> BExpr
handleBoolOp EqOp a b = Eq a b
handleBoolOp NeqOp a b = Neq a b
handleBoolOp LtOp a b = Lt a b
handleBoolOp LteOp a b = Lte a b
handleBoolOp GtOp a b = Gt a b
handleBoolOp GteOp a b = Gte a b
handleBoolOp _ _ _ = error "Invalid boolean operation"

-- Program reader
readProg :: [Token] -> Either [Instr] String
readProg tokens =
    let stack = sr [] tokens
    in case stack of
        [PI i] ->
            if isValidInstr i then Left [i] else parseError stack
        pis@(PI _ : _) ->
            let instrs = reverse (map (\(PI i) -> i) pis)
            in if all isValidInstr instrs then Left instrs else parseError stack
        _ -> parseError stack
  where
    parseError stk = Right $ "Parse error: " ++ show stk
    isValidInstr :: Instr -> Bool
    isValidInstr (While _ Nop) = False
    isValidInstr _             = True
-- Helper function to parse instructions
parseInstructions :: [Token] -> [Instr]
parseInstructions tokens =
    case sr [] tokens of
        pis@(PI _ : _) -> reverse (map (\(PI i) -> i) pis)
        _ -> []

-- The rest of your code remains the same...



-- TESTS BELOW DO NOT MODIFY BELOW THIS LINE
env1 :: Env
env1 = [("sum",10), ("y",3), ("i",5), ("acc",1),("c",3),("n",2)]
env2 :: Env
env2 = [("sum",20), ("y",100), ("i",5), ("acc",10),("c",4),("n",5)]

testLinesString =
  [ "(lexer \"sum := sum + y;\")"
  , "(lexer \"i := i + 2;\")"
  , "(lexer \"max := 100;\")"
  , "(lexer \"acc := acc * c;\")"
  , "(lexer \"return sum;\")"
  , "(lexer \"if (n==2) then return 1;\")"
  , "(lexer \"if (n % 2 == 0) then return 0;\")"
  , "(lexer \"while c <= fib - 1\")"
  , "(lexer \"while ! (fact <= c) {  c := c+1;   acc := acc * c; }\")"
  , "(lexer \"c:= c+1; z:= x+y; x:=y; y:=z;\")"
  , "(sr [] [VSym \"sum\",AssignOp,VSym \"sum\",BOp AddOp,VSym \"y\",Semi])"
  , "(sr [] [VSym \"i\",AssignOp,VSym \"i\",BOp AddOp,CSym 2,Semi])"
  , "(sr [] [VSym \"max\",AssignOp,CSym 100,Semi])"
  , "(sr [] [VSym \"acc\",AssignOp,VSym \"acc\",BOp MulOp,VSym \"c\",Semi])"
  , "(sr [] [Keyword ReturnK,VSym \"sum\",Semi])"
  , "(sr [] [Keyword IfK,LPar,VSym \"n\",BOp EqOp,CSym 2,RPar,Keyword ThenK,Keyword ReturnK,CSym 1,Semi])"
  , "(sr [] [Keyword IfK,LPar,VSym \"n\",BOp ModOp,CSym 2,BOp EqOp,CSym 0,RPar,Keyword ThenK,Keyword ReturnK,CSym 0,Semi])"
  , "(sr [] [Keyword WhileK,VSym \"c\",BOp LteOp,VSym \"fib\",BOp SubOp,CSym 1])"
  , "(sr [] [Keyword WhileK,NotOp,LPar,VSym \"fact\",BOp LteOp,VSym \"c\",RPar,LBra,VSym \"c\",AssignOp,VSym \"c\",BOp AddOp,CSym 1,Semi,VSym \"acc\",AssignOp,VSym \"acc\",BOp MulOp,VSym \"c\",Semi,RBra])"
  , "(sr [] [VSym \"c\",AssignOp,VSym \"c\",BOp AddOp,CSym 1,Semi,VSym \"z\",AssignOp,VSym \"x\",BOp AddOp,VSym \"y\",Semi,VSym \"x\",AssignOp,VSym \"y\",Semi,VSym \"y\",AssignOp,VSym \"z\",Semi])"
  , "(readProg [VSym \"sum\",AssignOp,VSym \"sum\",BOp AddOp,VSym \"y\",Semi])"
  , "(readProg [VSym \"i\",AssignOp,VSym \"i\",BOp AddOp,CSym 2,Semi])"
  , "(readProg [VSym \"max\",AssignOp,CSym 100,Semi])"
  , "(readProg [VSym \"acc\",AssignOp,VSym \"acc\",BOp MulOp,VSym \"c\",Semi])"
  , "(readProg [Keyword ReturnK,VSym \"sum\",Semi])"
  , "(readProg [Keyword IfK,LPar,VSym \"n\",BOp EqOp,CSym 2,RPar,Keyword ThenK,Keyword ReturnK,CSym 1,Semi])"
  , "(readProg [Keyword IfK,LPar,VSym \"n\",BOp ModOp,CSym 2,BOp EqOp,CSym 0,RPar,Keyword ThenK,Keyword ReturnK,CSym 0,Semi])"
  , "(readProg [Keyword WhileK,VSym \"c\",BOp LteOp,VSym \"fib\",BOp SubOp,CSym 1])"
  , "(readProg [Keyword WhileK,NotOp,LPar,VSym \"fact\",BOp LteOp,VSym \"c\",RPar,LBra,VSym \"c\",AssignOp,VSym \"c\",BOp AddOp,CSym 1,Semi,VSym \"acc\",AssignOp,VSym \"acc\",BOp MulOp,VSym \"c\",Semi,RBra])"
  , "(readProg [VSym \"c\",AssignOp,VSym \"c\",BOp AddOp,CSym 1,Semi,VSym \"z\",AssignOp,VSym \"x\",BOp AddOp,VSym \"y\",Semi,VSym \"x\",AssignOp,VSym \"y\",Semi,VSym \"y\",AssignOp,VSym \"z\",Semi])"
  , "(evala env1 (Add (Var \"sum\") (Var \"y\")))"
  , "(evala env1 (Add (Var \"i\") (Num 2)))"
  , "(evala env1 (Mul (Var \"acc\") (Var \"c\")))"
  , "(evala env1 (Add (Var \"c\") (Num 1)))"
  , "(evala env1 (Mod (Var \"n\") (Num 2)))"
  , "(evala env2 (Add (Var \"sum\") (Var \"y\")))"
  , "(evala env2 (Add (Var \"i\") (Num 2)))"
  , "(evala env2 (Mul (Var \"acc\") (Var \"c\")))"
  , "(evala env2 (Add (Var \"c\") (Num 1)))"
  , "(evala env2 (Mod (Var \"n\") (Num 2)))"
  , "(evalb env1 (Eq (Var \"n\") (Num 2)))"
  , "(evalb env1 (Eq (Mod (Var \"n\") (Num 2)) (Num 0)))"
  , "(evalb env1 (Not (Lte (Var \"sum\") (Var \"c\"))))"
  , "(evalb env1 (Not (Gte (Var \"sum\") (Var \"c\"))))"
  , "(evalb env1 (Lte (Var \"c\") (Sub (Var \"n\") (Num 1))))"
  , "(evalb env2 (Eq (Var \"n\") (Num 2)))"
  , "(evalb env2 (Eq (Mod (Var \"n\") (Num 2)) (Num 0)))"
  , "(evalb env2 (Not (Lte (Var \"sum\") (Var \"c\"))))"
  , "(evalb env2 (Not (Gte (Var \"sum\") (Var \"c\"))))"
  , "(evalb env2 (Lte (Var \"c\") (Sub (Var \"n\") (Num 1))))"
  , "(exec (Assign \"fact\" (Num 5)) [])"
  , "(exec (Assign \"acc\" (Num 1)) [(\"fact\",5)])"
  , "(exec (Assign \"c\" (Num 1)) [(\"fact\",5),(\"acc\",1)])"
  , "(exec (While (Not (Lte (Var \"fact\") (Var \"c\"))) (Do [Assign \"c\" (Add (Var \"c\") (Num 1)),Assign \"acc\" (Mul (Var \"acc\") (Var \"c\"))])) [(\"fact\",5),(\"acc\",1),(\"c\",1)])"
  , "(exec (Return (Var \"acc\")) [(\"fact\",5),(\"acc\",120),(\"c\",5)])"
  , "(execList [Assign \"c\" (Add (Var \"c\") (Num 1)),Assign \"acc\" (Mul (Var \"acc\") (Var \"c\"))] [(\"fact\",5),(\"acc\",1),(\"c\",1)])"
  , "(execList [Assign \"c\" (Add (Var \"c\") (Num 1)),Assign \"acc\" (Mul (Var \"acc\") (Var \"c\"))] [(\"fact\",5),(\"acc\",2),(\"c\",2)])"
  , "(execList [Assign \"c\" (Add (Var \"c\") (Num 1)),Assign \"acc\" (Mul (Var \"acc\") (Var \"c\"))] [(\"fact\",5),(\"acc\",6),(\"c\",3)])"
  , "(execList [Assign \"c\" (Add (Var \"c\") (Num 1)),Assign \"acc\" (Mul (Var \"acc\") (Var \"c\"))] [(\"fact\",5),(\"acc\",24),(\"c\",4)])"
  , "(execList [Assign \"fact\" (Num 5),Assign \"acc\" (Num 1),Assign \"c\" (Num 1),While (Not (Lte (Var \"fact\") (Var \"c\"))) (Do [Assign \"c\" (Add (Var \"c\") (Num 1)),Assign \"acc\" (Mul (Var \"acc\") (Var \"c\"))]),Return (Var \"acc\")] [])"
  ]

tests =
  [ ((lexer "sum := sum + y;") == [VSym "sum",AssignOp,VSym "sum",BOp AddOp,VSym "y",Semi])
  , ((lexer "i := i + 2;") == [VSym "i",AssignOp,VSym "i",BOp AddOp,CSym 2,Semi])
  , ((lexer "max := 100;") == [VSym "max",AssignOp,CSym 100,Semi])
  , ((lexer "acc := acc * c;") == [VSym "acc",AssignOp,VSym "acc",BOp MulOp,VSym "c",Semi])
  , ((lexer "return sum;") == [Keyword ReturnK,VSym "sum",Semi])
  , ((lexer "if (n==2) then return 1;") == [Keyword IfK,LPar,VSym "n",BOp EqOp,CSym 2,RPar,Keyword ThenK,Keyword ReturnK,CSym 1,Semi])
  , ((lexer "if (n % 2 == 0) then return 0;") == [Keyword IfK,LPar,VSym "n",BOp ModOp,CSym 2,BOp EqOp,CSym 0,RPar,Keyword ThenK,Keyword ReturnK,CSym 0,Semi])
  , ((lexer "while c <= fib - 1") == [Keyword WhileK,VSym "c",BOp LteOp,VSym "fib",BOp SubOp,CSym 1])
  , ((lexer "while ! (fact <= c) {  c := c+1;   acc := acc * c; }") == [Keyword WhileK,NotOp,LPar,VSym "fact",BOp LteOp,VSym "c",RPar,LBra,VSym "c",AssignOp,VSym "c",BOp AddOp,CSym 1,Semi,VSym "acc",AssignOp,VSym "acc",BOp MulOp,VSym "c",Semi,RBra])
  , ((lexer "c:= c+1; z:= x+y; x:=y; y:=z;") == [VSym "c",AssignOp,VSym "c",BOp AddOp,CSym 1,Semi,VSym "z",AssignOp,VSym "x",BOp AddOp,VSym "y",Semi,VSym "x",AssignOp,VSym "y",Semi,VSym "y",AssignOp,VSym "z",Semi])
  , ((sr [] [VSym "sum",AssignOp,VSym "sum",BOp AddOp,VSym "y",Semi]) == [PI (Assign "sum" (Add (Var "sum") (Var "y")))])
  , ((sr [] [VSym "i",AssignOp,VSym "i",BOp AddOp,CSym 2,Semi]) == [PI (Assign "i" (Add (Var "i") (Num 2)))])
  , ((sr [] [VSym "max",AssignOp,CSym 100,Semi]) == [PI (Assign "max" (Num 100))])
  , ((sr [] [VSym "acc",AssignOp,VSym "acc",BOp MulOp,VSym "c",Semi]) == [PI (Assign "acc" (Mul (Var "acc") (Var "c")))])
  , ((sr [] [Keyword ReturnK,VSym "sum",Semi]) == [PI (Return (Var "sum"))])
  , ((sr [] [Keyword IfK,LPar,VSym "n",BOp EqOp,CSym 2,RPar,Keyword ThenK,Keyword ReturnK,CSym 1,Semi]) == [PI (IfThen (Eq (Var "n") (Num 2)) (Return (Num 1)))])
  , ((sr [] [Keyword IfK,LPar,VSym "n",BOp ModOp,CSym 2,BOp EqOp,CSym 0,RPar,Keyword ThenK,Keyword ReturnK,CSym 0,Semi]) == [PI (IfThen (Eq (Mod (Var "n") (Num 2)) (Num 0)) (Return (Num 0)))])
  , ((sr [] [Keyword WhileK,VSym "c",BOp LteOp,VSym "fib",BOp SubOp,CSym 1]) == [PB (Lte (Var "c") (Sub (Var "fib") (Num 1))),Keyword WhileK])
  , ((sr [] [Keyword WhileK,NotOp,LPar,VSym "fact",BOp LteOp,VSym "c",RPar,LBra,VSym "c",AssignOp,VSym "c",BOp AddOp,CSym 1,Semi,VSym "acc",AssignOp,VSym "acc",BOp MulOp,VSym "c",Semi,RBra]) == [PI (While (Not (Lte (Var "fact") (Var "c"))) (Do [Assign "c" (Add (Var "c") (Num 1)),Assign "acc" (Mul (Var "acc") (Var "c"))]))])
  , ((sr [] [VSym "c",AssignOp,VSym "c",BOp AddOp,CSym 1,Semi,VSym "z",AssignOp,VSym "x",BOp AddOp,VSym "y",Semi,VSym "x",AssignOp,VSym "y",Semi,VSym "y",AssignOp,VSym "z",Semi]) == [PI (Assign "y" (Var "z")),PI (Assign "x" (Var "y")),PI (Assign "z" (Add (Var "x") (Var "y"))),PI (Assign "c" (Add (Var "c") (Num 1)))])
  , ((readProg [VSym "sum",AssignOp,VSym "sum",BOp AddOp,VSym "y",Semi]) == Left [Assign "sum" (Add (Var "sum") (Var "y"))])
  , ((readProg [VSym "i",AssignOp,VSym "i",BOp AddOp,CSym 2,Semi]) == Left [Assign "i" (Add (Var "i") (Num 2))])
  , ((readProg [VSym "max",AssignOp,CSym 100,Semi]) == Left [Assign "max" (Num 100)])
  , ((readProg [VSym "acc",AssignOp,VSym "acc",BOp MulOp,VSym "c",Semi]) == Left [Assign "acc" (Mul (Var "acc") (Var "c"))])
  , ((readProg [Keyword ReturnK,VSym "sum",Semi]) == Left [Return (Var "sum")])
  , ((readProg [Keyword IfK,LPar,VSym "n",BOp EqOp,CSym 2,RPar,Keyword ThenK,Keyword ReturnK,CSym 1,Semi]) == Left [IfThen (Eq (Var "n") (Num 2)) (Return (Num 1))])
  , ((readProg [Keyword IfK,LPar,VSym "n",BOp ModOp,CSym 2,BOp EqOp,CSym 0,RPar,Keyword ThenK,Keyword ReturnK,CSym 0,Semi]) == Left [IfThen (Eq (Mod (Var "n") (Num 2)) (Num 0)) (Return (Num 0))])
  , ((readProg [Keyword WhileK,VSym "c",BOp LteOp,VSym "fib",BOp SubOp,CSym 1]) == Right "Parse error: [RBra,PB (Lte (Var \"c\") (Sub (Var \"fib\") (Num 1))),Keyword WhileK,Block []]")
  , ((readProg [Keyword WhileK,NotOp,LPar,VSym "fact",BOp LteOp,VSym "c",RPar,LBra,VSym "c",AssignOp,VSym "c",BOp AddOp,CSym 1,Semi,VSym "acc",AssignOp,VSym "acc",BOp MulOp,VSym "c",Semi,RBra]) == Left [While (Not (Lte (Var "fact") (Var "c"))) (Do [Assign "c" (Add (Var "c") (Num 1)),Assign "acc" (Mul (Var "acc") (Var "c"))])])
  , ((readProg [VSym "c",AssignOp,VSym "c",BOp AddOp,CSym 1,Semi,VSym "z",AssignOp,VSym "x",BOp AddOp,VSym "y",Semi,VSym "x",AssignOp,VSym "y",Semi,VSym "y",AssignOp,VSym "z",Semi]) == Left [Assign "c" (Add (Var "c") (Num 1)),Assign "z" (Add (Var "x") (Var "y")),Assign "x" (Var "y"),Assign "y" (Var "z")])
  , ((evala env1 (Add (Var "sum") (Var "y"))) == 13)
  , ((evala env1 (Add (Var "i") (Num 2))) == 7)
  , ((evala env1 (Mul (Var "acc") (Var "c"))) == 3)
  , ((evala env1 (Add (Var "c") (Num 1))) == 4)
  , ((evala env1 (Mod (Var "n") (Num 2))) == 0)
  , ((evala env2 (Add (Var "sum") (Var "y"))) == 120)
  , ((evala env2 (Add (Var "i") (Num 2))) == 7)
  , ((evala env2 (Mul (Var "acc") (Var "c"))) == 40)
  , ((evala env2 (Add (Var "c") (Num 1))) == 5)
  , ((evala env2 (Mod (Var "n") (Num 2))) == 1)
  , ((evalb env1 (Eq (Var "n") (Num 2))) == True)
  , ((evalb env1 (Eq (Mod (Var "n") (Num 2)) (Num 0))) == True)
  , ((evalb env1 (Not (Lte (Var "sum") (Var "c")))) == True)
  , ((evalb env1 (Not (Gte (Var "sum") (Var "c")))) == False)
  , ((evalb env1 (Lte (Var "c") (Sub (Var "n") (Num 1)))) == False)
  , ((evalb env2 (Eq (Var "n") (Num 2))) == False)
  , ((evalb env2 (Eq (Mod (Var "n") (Num 2)) (Num 0))) == False)
  , ((evalb env2 (Not (Lte (Var "sum") (Var "c")))) == True)
  , ((evalb env2 (Not (Gte (Var "sum") (Var "c")))) == False)
  , ((evalb env2 (Lte (Var "c") (Sub (Var "n") (Num 1)))) == True)
  , ((exec (Assign "fact" (Num 5)) []) == [("fact",5)])
  , ((exec (Assign "acc" (Num 1)) [("fact",5)]) == [("fact",5),("acc",1)])
  , ((exec (Assign "c" (Num 1)) [("fact",5),("acc",1)]) == [("fact",5),("acc",1),("c",1)])
  , ((exec (While (Not (Lte (Var "fact") (Var "c"))) (Do [Assign "c" (Add (Var "c") (Num 1)),Assign "acc" (Mul (Var "acc") (Var "c"))])) [("fact",5),("acc",1),("c",1)]) == [("fact",5),("acc",120),("c",5)])
  , ((exec (Return (Var "acc")) [("fact",5),("acc",120),("c",5)]) == [("",120)])
  , ((execList [Assign "c" (Add (Var "c") (Num 1)),Assign "acc" (Mul (Var "acc") (Var "c"))] [("fact",5),("acc",1),("c",1)]) == [("fact",5),("acc",2),("c",2)])
  , ((execList [Assign "c" (Add (Var "c") (Num 1)),Assign "acc" (Mul (Var "acc") (Var "c"))] [("fact",5),("acc",2),("c",2)]) == [("fact",5),("acc",6),("c",3)])
  , ((execList [Assign "c" (Add (Var "c") (Num 1)),Assign "acc" (Mul (Var "acc") (Var "c"))] [("fact",5),("acc",6),("c",3)]) == [("fact",5),("acc",24),("c",4)])
  , ((execList [Assign "c" (Add (Var "c") (Num 1)),Assign "acc" (Mul (Var "acc") (Var "c"))] [("fact",5),("acc",24),("c",4)]) == [("fact",5),("acc",120),("c",5)])
  , ((execList [Assign "fact" (Num 5),Assign "acc" (Num 1),Assign "c" (Num 1),While (Not (Lte (Var "fact") (Var "c"))) (Do [Assign "c" (Add (Var "c") (Num 1)),Assign "acc" (Mul (Var "acc") (Var "c"))]),Return (Var "acc")] []) == [("",120)])
  ]

main = do
  putStrLn $ show (length (filter id tests)) ++ '/' : show (length tests)
  let zipped = zip tests testLinesString
  sequence (map (print . snd) (filter (not . fst) zipped))
