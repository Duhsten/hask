type Vars = String          -- Variables
type Value = Integer        -- Values
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



data Keywords = IfK | ThenK | ElseK | WhileK | NopK | ReturnK | ForK
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

-- integer constants
lexer (x:xs) | isDigit x =
  let (y,z) = span isDigit (x:xs)
   in CSym (read y) : lexer z

-- boolean constants
lexer xs | take 4 xs == "true"  = BSym True : lexer (drop 4 xs)
lexer xs | take 5 xs == "false" = BSym False : lexer (drop 5 xs)

-- binary operators
lexer xs | take 4 xs == "else" = Keyword ElseK : lexer (drop 4 xs)
lexer xs | take 2 xs == "if"   = Keyword IfK : lexer (drop 2 xs)
lexer xs | take 6 xs == "return" = Keyword ReturnK : lexer (drop 6 xs)
lexer ('f':'o':'r':xs) = Keyword ForK : lexer xs
lexer (':':'=':xs) = AssignOp : lexer xs
lexer ('=':'=':xs) = BOp EqOp : lexer xs
lexer ('=':xs) = AssignOp : lexer xs
lexer ('+':xs) = BOp AddOp : lexer xs
lexer ('<':xs) = BOp LtOp  : lexer xs
lexer ('%':xs) = BOp ModOp : lexer xs
lexer ('|':'|':xs) = BOp OrOp : lexer xs
lexer ('&':'&':xs) = BOp AndOp : lexer xs
lexer ('!':'=':xs) = BOp NeqOp : lexer xs
lexer ('>':'=':xs) = BOp GteOp : lexer xs
lexer ('>':xs) = BOp GtOp : lexer xs
lexer ('<':'=':xs) = BOp LteOp : lexer xs
lexer ('*':xs) = BOp MulOp : lexer xs
lexer ('/':xs) = BOp DivOp : lexer xs
lexer ('-':xs) = BOp SubOp : lexer xs
lexer ('^':xs) = BOp ExpOp : lexer xs
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
lexer (x:xs) | elem x " \t\n" = lexer xs
lexer "" = []
lexer xs = [Err (take 10 xs)]
