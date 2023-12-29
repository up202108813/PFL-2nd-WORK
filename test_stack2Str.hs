import Data.List (sort)
import Data.Char (isSpace, isAlpha, isDigit, isAlphaNum)

-- type Stack = [String]

-- createEmptyStack :: Stack
-- createEmptyStack = []

-- stack2Str :: Stack -> String
-- stack2Str stack =
--   let myList = concatMap showValue stack
--   in if null myList then [] else init myList
--   where showValue var = var ++ ","

-- printStack :: Stack -> String -> IO ()
-- printStack stack expected = putStrLn (stack2Str stack ++ if stack2Str stack == expected then " Test Passed" else " Test Failed")

-- type State = [(String, String)]

-- createEmptyState :: State
-- createEmptyState = []

-- state2Str :: State -> String
-- state2Str state =
--   let myList = concatMap showPair $ sort state
--   in if null myList then [] else init myList
--   where showPair (var, value) = var ++ "=" ++ value ++ ","

-- printState :: State -> String -> IO ()
-- printState state expected = putStrLn (state2Str state ++ if state2Str state == expected then " Test Passed" else " Test Failed")

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
  | EqTok Aexp Aexp     -- Represents equality comparison of two arithmetic expressions
  | LtTok Aexp Aexp     -- Represents less than comparison of two arithmetic expressions
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
    ";" : restTokens -> parseAux restTokens
    (var : ":=" : restTokens) ->
      let (expression, tokensAfterExpression) = parseAexp restTokens
      in Assign var expression : parseAux tokensAfterExpression
    _ -> []

parseAexp :: [String] -> (Aexp, [String])
parseAexp tokens =
  let (exp1, tokensAfterExp1) = parseTerm tokens
  in parseAexpAux exp1 tokensAfterExp1

parseAexpAux :: Aexp -> [String] -> (Aexp, [String])
parseAexpAux exp1 (op:tokens) =
  case op of
    "+" ->
      let (exp2, tokensAfterExp2) = parseTerm tokens
          (exp3, tokensAfterExp3) = parseAexpAux (AddTok exp1 exp2) tokensAfterExp2
      in (exp3, tokensAfterExp3)
    "-" ->
      let (exp2, tokensAfterExp2) = parseTerm tokens
          (exp3, tokensAfterExp3) = parseAexpAux (SubTok exp1 exp2) tokensAfterExp2
      in (exp3, tokensAfterExp3)
    "*" ->
      let (exp2, tokensAfterExp2) = parseTerm tokens
          (exp3, tokensAfterExp3) = parseAexpAux (MulTok exp1 exp2) tokensAfterExp2
      in (exp3, tokensAfterExp3)
    _ -> (exp1, op:tokens)
parseAexpAux exp tokens = (exp, tokens)

parseTerm :: [String] -> (Aexp, [String])
parseTerm (token:tokens) =
  parseFactor (token:tokens)


parseFactor :: [String] -> (Aexp, [String])
parseFactor (token:tokens) =
  case token of
    "-" ->
      let (exp, tokensAfterExp) = parseFactor tokens
      in (MulTok (Num (-1)) exp, tokensAfterExp)
    "(" ->
      let (exp, tokensAfterExp) = parseAexp tokens
      in case tokensAfterExp of
           (")":restTokens) -> (exp, restTokens)
           _ -> error "Missing closing parenthesis"
    var ->
      if all isAlpha var
        then (Var var, tokens)
        else (Num (read var :: Integer), tokens)

-- Function to tokenize a string
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

-- Example usage:
main :: IO ()
main = do
    -- let myStack = ["-20","True","False"]
    -- let expectedStack = "False,True,-20"
    -- let myState = [("z", "10"), ("y", "20"), ("x", "30")]
    -- let expectedState = "x=30,y=20,z=10"
    -- printStack myStack expectedStack
    -- printState myState expectedState
    let input2 = "x := 0 - 2;"
    print $ parse input2

