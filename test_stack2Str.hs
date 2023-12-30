import Data.List (sort)
import Data.Char (isSpace, isAlpha, isDigit, isAlphaNum)

data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

data Aexp
  = Num Integer         -- Represents a numeric constant
  | Var String          -- Represents a variable
  | AddTok Aexp Aexp    -- Represents addition of two arithmetic expressions
  | SubTok Aexp Aexp    -- Represents subtraction of two arithmetic expressions
  | MulTok Aexp Aexp    -- Represents multiplication of two arithmetic expressions
  deriving (Show)

data Bexp
  = BoolLit Bool     -- Represents a boolean constant
  | NotTok Bexp         -- Represents negation of a boolean expression
  | AndTok Bexp Bexp    -- Represents logical AND of two boolean expressions
  | OrTok Bexp Bexp     -- Represents logical OR of two boolean expressions
  | EqArTok Aexp Aexp     -- Represents equality comparison of two arithmetic expressions
  | EqBolTok Bexp Bexp     -- Represents equality comparison of two boolean expressions
  | LeTok Aexp Aexp     -- Represents less than comparison of two arithmetic expressions
  deriving (Show)

data Stm
  = Assign String Aexp   -- Represents assignment of an arithmetic expression to a variable
  | If Bexp Stm Stm      -- Represents conditional statement
  | While Bexp Stm       -- Represents while loop
  | Seq [Stm]            -- Represents sequence of statements
  deriving (Show)

type Program = [Stm]

-- Function to parse a string into a program
parse :: String -> Program
parse string =
  let tokens = lexer string
  in parseAux tokens

parseAux :: [String] -> Program
parseAux tokens =
  case tokens of
    [] -> []
    (";" : restTokens) -> parseAux restTokens
    (var : ":=" : restTokens) ->
      let (expression, tokensAfterExpression) = parseAexp restTokens
      in Assign var expression : parseAux tokensAfterExpression

    ("if" : restTokens) ->
      let
        (bexp, tokensAfterBexp) = parseBexp restTokens -- parse boolean expression
        -- ("then" : tokensAfterThen) = tokensAfterBexp -- check for "then" token
        -- stm1 = parseAux tokensAfterThen   
        -- ("else" : tokensAfterElse) = tokensAfterStm1
        -- (stm2, tokensAfterStm2) = parseAux tokensAfterElse
      in If bexp (Assign "x" (Num 5)) (Assign "y" (Num 7)) : parseAux tokensAfterBexp
    _ -> []

-------------------------------------------------------------------------------------------
-- BOOLEAN EXPRESSION PARSER
-------------------------------------------------------------------------------------------
parseBexp :: [String] -> (Bexp, [String])
parseBexp tokens =
  let (exp1, tokensAfterExp1) = parseBexpFactor tokens
  in parseBexpAux exp1 tokensAfterExp1

parseBexpAux :: Bexp -> [String] -> (Bexp, [String])
parseBexpAux exp1 (op:tokens) =
  case op of
    "=" ->
        let (exp2, tokensAfterExp2) = parseBexpFactor tokens
            (exp3, tokensAfterExp3) = parseBexpAux (EqBolTok exp1 exp2) tokensAfterExp2
        in (exp3, tokensAfterExp3)
    "and" ->
        let (exp2, tokensAfterExp2) = parseBexpFactor tokens
            (exp3, tokensAfterExp3) = parseBexpAux (AndTok exp1 exp2) tokensAfterExp2
        in (exp3, tokensAfterExp3)
    "or" ->
        let (exp2, tokensAfterExp2) = parseBexpFactor tokens
            (exp3, tokensAfterExp3) = parseBexpAux (OrTok exp1 exp2) tokensAfterExp2
        in (exp3, tokensAfterExp3)
    -- "<=" ->
    --     let (exp1, tokensAfterExp1) = parseAexpFactor tokens
    --         (exp2, tokensAfterExp2) = parseAexpFactor tokensAfterExp1
    --     in (LeTok exp1 exp2, tokensAfterExp2)
    -- "==" ->
    --     let (exp1, tokensAfterExp1) = parseAexpFactor tokens
    --         (exp2, tokensAfterExp2) = parseAexpFactor tokensAfterExp1
    --     in (EqArTok exp1 exp2, tokensAfterExp2)
    _ -> (exp1, op:tokens)
parseBexpAux exp tokens = (exp, tokens)

parseBexpFactor :: [String] -> (Bexp, [String])
parseBexpFactor (token:tokens) =
  case token of
    "(" ->
      let (exp, tokensAfterExp) = parseBexp tokens
      in case tokensAfterExp of
           (")":restTokens) -> (exp, restTokens)
           _ -> error "Missing closing parenthesis"
    "True" -> (BoolLit True, tokens)
    "False" -> (BoolLit False, tokens)
    "not" ->
      let (exp, tokensAfterExp) = parseBexp tokens
      in (NotTok exp, tokensAfterExp)
    var -> error ("Invalid boolean expression: " ++ show var)

-------------------------------------------------------------------------------------------
-- ARITHMETIC EXPRESSION PARSER  
-------------------------------------------------------------------------------------------
parseAexp :: [String] -> (Aexp, [String])
parseAexp tokens =
  let (exp1, tokensAfterExp1) = parseAexpFactor tokens
  in parseAexpAux exp1 tokensAfterExp1

parseAexpAux :: Aexp -> [String] -> (Aexp, [String])
parseAexpAux exp1 (op:tokens) =
  case op of
    "*" ->
      let (exp2, tokensAfterExp2) = parseAexpFactor tokens
          (exp3, tokensAfterExp3) = parseAexpAux (MulTok exp1 exp2) tokensAfterExp2
      in (exp3, tokensAfterExp3)
    "+" ->
      let (exp2, tokensAfterExp2) = parseAexpFactor tokens
          (exp3, tokensAfterExp3) = parseAexpAux (AddTok exp1 exp2) tokensAfterExp2
      in (exp3, tokensAfterExp3)
    "-" ->
      let (exp2, tokensAfterExp2) = parseAexpFactor tokens
          (exp3, tokensAfterExp3) = parseAexpAux (SubTok exp1 exp2) tokensAfterExp2
      in (exp3, tokensAfterExp3)
    _ -> (exp1, op:tokens)
parseAexpAux exp tokens = (exp, tokens)

parseAexpFactor :: [String] -> (Aexp, [String])
parseAexpFactor (token:tokens) =
  case token of
    "(" ->
      let (exp, tokensAfterExp) = parseAexp tokens
      in case tokensAfterExp of
           (")":restTokens) -> (exp, restTokens)
           _ -> error "Missing closing parenthesis"
    var ->
      if all isAlpha var
        then (Var var, tokens)
        else (Num (read var :: Integer), tokens)

-------------------------------------------------------------------------------------------
-- LEXER
-----------------------------------------------------------------------------------------
lexer :: String -> [String]
lexer = go []
  where
    go acc [] = reverse acc
    go acc (c:cs)
      | isSpace c = go acc cs
      | isAlpha c = let (token, rest) = span isAlphaNum (c:cs) in go (classify token : acc) rest
      | isDigit c =
          let (token, rest) = span (\x -> isDigit x || x == '.') (c:cs)
          in go (token : acc) rest
      | c == ':' && head cs == '=' = go (":=" : acc) (tail cs)
      | c == '<' && head cs == '=' = go ("<=" : acc) (tail cs)
      | c == '=' && head cs == '=' = go ("==" : acc) (tail cs)
      | otherwise = go ([c] : acc) cs

    classify token
      | token `elem` ["while", "if", "then", "else", "not", "and", "or", "do"] = token
      | token `elem` ["+", "-", "*", ":=", "<", "<=", "==", "="] = token
      | token `elem` [";", "(", ")"] = token
      | token `elem` ["True", "False"] = token
      | otherwise = token



compA :: Aexp -> Code
compA (Num n) = [Push n]
compA (Var v) = [Fetch v]
compA (AddTok a1 a2) = compA a1 ++ compA a2 ++ [Add]
compA (SubTok a1 a2) = compA a1 ++ compA a2 ++ [Sub]
compA (MulTok a1 a2) = compA a1 ++ compA a2 ++ [Mult]


-- compB :: Bexp -> Code
-- compB (BoolLit True) = [Push 1]
-- compB (BoolLit False) = [Push 0]
-- compB (NotTok b) = compB b ++ [Not]
-- compB (AndTok b1 b2) = compB b1 ++ compB b2 ++ [And]
-- compB (OrTok b1 b2) = compB b1 ++ compB b2 ++ [Or]


compile :: Program -> Code
compile [] = []
compile (stmt:stmts) =
  case stmt of
    (Assign var aexp) -> compA aexp ++ [Store var] ++ compile stmts
    -- add other cases here if needed

-------------------------------------------------------------------------------------------
-- MAIN
-------------------------------------------------------------------------------------------
main :: IO ()
main = do
    let input2 = "i := 5; if True then x := 5 else y := 7"
    print $ parse input2
