import Data.List (sort)
import Data.Char (isSpace, isAlpha, isDigit, isAlphaNum, isNumber)


-- Part 1
-- Do not modify our definition of Inst and Code
data Inst =
  Push Integer | Add | Mult | Sub | Tru | Fals | Equ | Le | And | Neg | Fetch String | Store String | Noop |
  Branch Code Code | Loop Code Code
  deriving Show
type Code = [Inst]

-- Stack is defined as a list where the fisrt element corresponds to the top of the stack
type Stack = [String]

createEmptyStack :: Stack
createEmptyStack = []

stack2Str :: Stack -> String
stack2Str stack =
  let myList = concatMap showValue stack
  in if null myList then [] else init myList
  where showValue var = var ++ ","

type State = [(String, String)]

createEmptyState :: State
createEmptyState = []

state2Str :: State -> String
state2Str state =
  let myList = concatMap showPair $ sort state
  in if null myList then [] else init myList
  where showPair (var, value) = var ++ "=" ++ value ++ ","


run :: (Code, Stack, State) -> (Code, Stack, State)

-- Base case: empty code list, return the current stack and state
run ([], stack, state) = ([], stack, state)

-- Push instruction: push the value onto the stack
run ((Push n):code, stack, state) = run (code, show n:stack, state)

-- Add instruction: pop two values from the stack, add them, and push the result
run (Add:code, x:y:stack, state) = run (code, show (read x + read y):stack, state)

-- Mult instruction: pop two values from the stack, multiply them, and push the result
run (Mult:code, x:y:stack, state) = run (code, show (read x * read y):stack, state)

-- Sub instruction: pop two values from the stack, subtract them, and push the result
run (Sub:code, x:y:stack, state) = run (code, show (read x - read y):stack, state)

-- Tru instruction: push the string "True" onto the stack
run (Tru:code, stack, state) = run (code, "True":stack, state)

-- Fals instruction: push the string "False" onto the stack
run (Fals:code, stack, state) = run (code, "False":stack, state)

-- Equ instruction: pop two values from the stack, compare them for equality, and push the result
run (Equ:code, x:y:stack, state) = run (code, show (x == y):stack, state)

-- Le instruction: pop two values from the stack, compare them for less than or equal, and push the result
run (Le:code, x:y:stack, state) = run (code, show ((read x :: Integer) <= (read y :: Integer)):stack, state)

-- And instruction: pop two values from the stack, compare them for logical and, and push the result
run (And:code, x:y:stack, state)
  | x `elem` ["True", "False"] && y `elem` ["True", "False"] = run (code, show (read x && read y):stack, state)
  | otherwise = error "Run-time error" -- Raise a run-time error for invalid operands

-- Neg instruction: pop a value from the stack, negate it, and push the result
run (Neg:code, x:stack, state)
  | x `elem` ["True", "False"] = run (code, show (not (read x)):stack, state)
  | otherwise = error "Run-time error" -- Raise a run-time error for invalid operand

-- Fetch instruction: push the value associated with the variable onto the stack
run ((Fetch var):code, stack, state) = run (code, value:stack, state)
  where value = case lookup var state of
                  Just v -> v
                  Nothing -> error ("Run-time error: variable " ++ var ++ " not found") -- Raise a run-time error if the variable is not found in the state

-- Store instruction: pop a value from the stack and store it in the state with the given variable name
run ((Store var):code, [], state) = error "Run-time error: empty stack" -- Raise a run-time error if the stack is empty

-- Store instruction: pop a value from the stack and store it in the state with the given variable name
run ((Store var):code, value:stack, state) =
  let newState = (var, value):filter (\(k, _) -> k /= var) state -- Create a new state by adding the variable-value pair to the state and removing any existing pairs with the same variable name
  in run (code, stack, newState)

-- Noop instruction: do nothing
run (Noop:code, stack, state) = run (code, stack, state)

-- Branch instruction: pop a value from the stack, if it's "True" execute c1, otherwise execute c2
run ((Branch c1 c2):code, x:stack, state) = if x == "True" then run (c1 ++ code, stack, state) else run (c2 ++ code, stack, state)

-- Loop instruction: pop a value from the stack, if it's "True" execute c2 and then execute the loop again, otherwise continue executing the code
run ((Loop c1 c2):code, stack, state) =
  let (_, x:stack', state') = run (c1, stack, state) -- Moves to the top of the stack true or false depending on if the condition from c1 is met or not
  in run (if x == "True" then c2 ++ Loop c1 c2:code else code, stack', state') -- If the top of the stack is "True", execute c2 and continue running the code with the updated stack and state

-- Invalid instruction: raise an error
run _instruction = error ("Invalid instruction: " ++ show _instruction)

-- To help you test your assembler
testAssembler :: Code -> (String, String)
testAssembler code = (stack2Str stack, state2Str state)
  where (_,stack,state) = run (code, createEmptyStack, createEmptyState)

test :: String -> String -> String -> String -> IO ()
test testName received expected passed = do
  putStrLn $ testName ++ ":"
  putStrLn $ "  Received:   " ++ received
  putStrLn $ "  Expected:   " ++ expected
  putStrLn $ "  Passed Test:   " ++ passed

-- Part 2
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
compA (AddTok a1 a2) = compA a2 ++ compA a1 ++ [Add]
compA (SubTok a1 a2) = compA a2 ++ compA a1 ++ [Sub]
compA (MulTok a1 a2) = compA a2 ++ compA a1 ++ [Mult]

compB :: Bexp -> Code
compB (BoolLit b) = if b then [Tru] else [Fals]
compB (NotTok b) = compB b ++ [Neg]
compB (AndTok b1 b2) = compB b2 ++ compB b1 ++ [And]
compB (EqArTok a1 a2) = compA a2 ++ compA a1 ++ [Equ]
compB (EqBolTok b1 b2) = compB b2 ++ compB b1 ++ [Equ]
compB (LeTok a1 a2) = compA a2 ++ compA a1 ++ [Le]


compile :: Program -> Code
compile [] = []
compile (stmt:stmts) =
  case stmt of
    (Assign var aexp) -> compA aexp ++ [Store var] ++ compile stmts
    (If bexp stm1 stm2) -> compB bexp ++ [Branch (compile [stm1]) (compile [stm2])] ++ compile stmts
    (While bexp stm) -> compB bexp ++ [Loop (compile [stm]) (compile [stm])] ++ compile stmts

parse :: String -> Program
parse string =
  let tokens = fixMultiplication . fixMultiplication $ lexer string
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

parseBexp :: [String] -> (Bexp, [String])
parseBexp tokens =
  let (exp1, tokensAfterExp1) = parseBexpTerm tokens
  in parseBexpAux exp1 tokensAfterExp1

parseBexpAux :: Bexp -> [String] -> (Bexp, [String])
parseBexpAux exp1 (op:tokens) =
  case op of
    "=" ->
        let (exp2, tokensAfterExp2) = parseBexpTerm tokens
            (exp3, tokensAfterExp3) = parseBexpAux (EqBolTok exp1 exp2) tokensAfterExp2
        in (exp3, tokensAfterExp3)
    "and" ->
        let (exp2, tokensAfterExp2) = parseBexpTerm tokens
            (exp3, tokensAfterExp3) = parseBexpAux (AndTok exp1 exp2) tokensAfterExp2
        in (exp3, tokensAfterExp3)
    "<=" ->
        let (exp1, tokensAfterExp1) = parseAexpTerm tokens
            (exp2, tokensAfterExp2) = parseAexpTerm tokensAfterExp1
        in (LeTok exp1 exp2, tokensAfterExp2)
    "==" ->
        let (exp1, tokensAfterExp1) = parseAexpTerm tokens
            (exp2, tokensAfterExp2) = parseAexpTerm tokensAfterExp1
        in (EqArTok exp1 exp2, tokensAfterExp2)
    _ -> (exp1, op:tokens)
parseBexpAux exp tokens = (exp, tokens)

parseBexpTerm :: [String] -> (Bexp, [String])
parseBexpTerm (token:tokens) =
  parseBexpFactor (token:tokens)

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
  let (exp1, tokensAfterExp1) = parseAexpTerm tokens
  in parseAexpAux exp1 tokensAfterExp1

parseAexpAux :: Aexp -> [String] -> (Aexp, [String])
parseAexpAux exp1 (op:tokens) =
  case op of
    "+" ->
      let (exp2, tokensAfterExp2) = parseAexpTerm tokens
          (exp3, tokensAfterExp3) = parseAexpAux (AddTok exp1 exp2) tokensAfterExp2
      in (exp3, tokensAfterExp3)
    "-" ->
      let (exp2, tokensAfterExp2) = parseAexpTerm tokens
          (exp3, tokensAfterExp3) = parseAexpAux (SubTok exp1 exp2) tokensAfterExp2
      in (exp3, tokensAfterExp3)
    "*" ->
      let (exp2, tokensAfterExp2) = parseAexpTerm tokens
          (exp3, tokensAfterExp3) = parseAexpAux (MulTok exp1 exp2) tokensAfterExp2
      in (exp3, tokensAfterExp3)
    _ -> (exp1, op:tokens)
parseAexpAux exp tokens = (exp, tokens)

parseAexpTerm :: [String] -> (Aexp, [String])
parseAexpTerm (token:tokens) =
  parseAexpFactor (token:tokens)

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

fixMultiplication :: [String] -> [String]
fixMultiplication (x:op:y:xs)
    | (isNumber (head x) || isAlpha (head x))  && (isNumber (head y) || isAlpha (head y)) && op == "*" = ["("] ++ [x] ++ [op] ++ [y] ++ [")"] ++ fixMultiplication xs
    | x == "(" && y == ")" && op /= "True" && op /= "False" = fixMultiplication [op] ++ xs
    | otherwise = x : fixMultiplication (op : y : xs)
fixMultiplication [x] = [x] -- Add pattern match for single element
fixMultiplication [] = [] -- Catch-all pattern for empty list
fixMultiplication other = other -- Handle the case with more than three elements

-- To help you test your parser
testParser :: String -> (String, String)
testParser programCode = (stack2Str stack, state2Str state)
  where (_,stack,state) = run (compile (parse programCode), createEmptyStack, createEmptyState)

main :: IO ()
main = do
    -- Part 1 tests
    putStrLn "=== Part 1 tests ==="

    let instructions = [Push 10,Push 4,Push 3,Sub,Mult]
    let received = show (run (instructions, createEmptyStack, createEmptyState))
    let expected = ("-10","")
    let passed = show (testAssembler instructions == expected)
    test "Test 1" received (show expected) passed

    let instructions = [Fals,Push 3,Tru,Store "var",Store "a", Store "someVar"]
    let received = show (run (instructions, createEmptyStack, createEmptyState))
    let expected = ("", "a=3,someVar=False,var=True")
    let passed = show (testAssembler instructions == expected)
    test "Test 2" received (show expected) passed

    let instructions = [Fals,Store "var",Fetch "var"]
    let received = show (run (instructions, createEmptyStack, createEmptyState))
    let expected = ("False","var=False")
    let passed = show (testAssembler instructions == expected)
    test "Test 3" received (show expected) passed

    let instructions = [Push (-20),Tru,Fals]
    let received = show (run (instructions, createEmptyStack, createEmptyState))
    let expected = ("False,True,-20","")
    let passed = show (testAssembler instructions == expected)
    test "Test 4" received (show expected) passed

    let instructions = [Push (-20),Tru,Tru,Neg]
    let received = show (run (instructions, createEmptyStack, createEmptyState))
    let expected = ("False,True,-20","")
    let passed = show (testAssembler instructions == expected)
    test "Test 5" received (show expected) passed

    let instructions = [Push (-20),Tru,Tru,Neg,Equ]
    let received = show (run (instructions, createEmptyStack, createEmptyState))
    let expected = ("False,-20","")
    let passed = show (testAssembler instructions == expected)
    test "Test 6" received (show expected) passed

    let instructions = [Push (-20),Push (-21), Le]
    let received = show (run (instructions, createEmptyStack, createEmptyState))
    let expected = ("True","")
    let passed = show (testAssembler instructions == expected)
    test "Test 7" received (show expected) passed

    let instructions = [Push 5,Store "x",Push 1,Fetch "x",Sub,Store "x"]
    let received = show (run (instructions, createEmptyStack, createEmptyState))
    let expected = ("","x=4")
    let passed = show (testAssembler instructions == expected)
    test "Test 8" received (show expected) passed

    let instructions = [Push 10,Store "i",Push 1,Store "fact",Loop [Push 1,Fetch "i",Equ,Neg] [Fetch "i",Fetch "fact",Mult,Store "fact",Push 1,Fetch "i",Sub,Store "i"]]
    let received = show (run (instructions, createEmptyStack, createEmptyState))
    let expected = ("","fact=3628800,i=1")
    let passed = show (testAssembler instructions == expected)
    test "Test 9" received (show expected) passed


    -- Execeptions
    -- let instructions = [Push 1,Push 2,And]
    -- let received = show (run (instructions, createEmptyStack, createEmptyState))
    -- let expected = ("Run-time error","")
    -- let passed = show (testAssembler instructions == expected)
    -- test "Test 10" received (show expected) passed

    -- let instructions = [Tru,Tru,Store "y", Fetch "x",Tru]
    -- let received = show (run (instructions, createEmptyStack, createEmptyState))
    -- let expected = ("Run-time error","")
    -- let passed = show (testAssembler instructions == expected)
    -- test "Test 11" received (show expected) passed


    -- Part 2 tests
    putStrLn "=== Part 2 tests ==="

    let string = "x := 5; x := x - 1;"
    let instructions = compile (parse string)
    let received = show (run (instructions, createEmptyStack, createEmptyState))
    let expected = ("","x=4")
    let passed = show (testParser string == expected)
    test "Test 12" received (show expected) passed

    let string = "x := 0 - 2;"
    let instructions = compile (parse string)
    let received = show (run (instructions, createEmptyStack, createEmptyState))
    let expected = ("","x=-2")
    let passed = show (testParser string == expected)
    test "Test 13" received (show expected) passed

    let string = "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);"
    let instructions = compile (parse string)
    let received = show (run (instructions, createEmptyStack, createEmptyState))
    let expected = ("","x=2,y=-10,z=6")
    let passed = show (testParser string == expected)
    test "Test 22" received (show expected) passed

    let string = "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;"
    let instructions = compile (parse string)
    let received = show (run (instructions, createEmptyStack, createEmptyState))
    let expected = ("","y=2")
    let passed = show (testParser string == expected)
    test "Test 14" received (show expected) passed

    let string = "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);"
    let instructions = compile (parse string)
    let received = show (run (instructions, createEmptyStack, createEmptyState))
    let expected = ("","x=1")
    let passed = show (testParser string == expected)
    test "Test 15" received (show expected) passed

    let string = "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;"
    let instructions = compile (parse string)
    let received = show (run (instructions, createEmptyStack, createEmptyState))
    let expected = ("","x=2")
    let passed = show (testParser string == expected)
    test "Test 16" received (show expected) passed

    let string = "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;"
    let instructions = compile (parse string)
    let received = show (run (instructions, createEmptyStack, createEmptyState))
    let expected = ("","x=2,z=4")
    let passed = show (testParser string == expected)
    test "Test 17" received (show expected) passed

    let string = "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;"
    let instructions = compile (parse string)
    let received = show (run (instructions, createEmptyStack, createEmptyState))
    let expected = ("","x=34,y=68")
    let passed = show (testParser string == expected)
    test "Test 18" received (show expected) passed

    let string = "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;"
    let instructions = compile (parse string)
    let received = show (run (instructions, createEmptyStack, createEmptyState))
    let expected = ("","x=34")
    let passed = show (testParser string == expected)
    test "Test 19" received (show expected) passed

    let string = "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;"
    let instructions = compile (parse string)
    let received = show (run (instructions, createEmptyStack, createEmptyState))
    let expected = ("","x=1")
    let passed = show (testParser string == expected)
    test "Test 20" received (show expected) passed

    let string = "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;"
    let instructions = compile (parse string)
    let received = show (run (instructions, createEmptyStack, createEmptyState))
    let expected = ("","x=2")
    let passed = show (testParser string == expected)
    test "Test 21" received (show expected) passed
    let string = "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);"

    let instructions = compile (parse string)
    let received = show (run (instructions, createEmptyStack, createEmptyState))
    let expected = ("","fact=3628800,i=1")
    let passed = show (testParser string == expected)
    test "Test 23" received (show expected) passed
