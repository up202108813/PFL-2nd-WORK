import Data.List (sort)
import Data.Char (isDigit, isSpace)
-- PFL 2023/24 - Haskell practical assignment quickstart
-- Updated on 27/12/2023

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

main :: IO ()
main = do
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

    -- let instructions = [Push 1,Push 2,And]
    -- let received = show (run (instructions, createEmptyStack, createEmptyState))
    -- let expected = ("Run-time error","")
    -- let passed = show (testAssembler instructions == expected)
    -- test "Test 10" received (show expected) passed

    let instructions = [Tru,Tru,Store "y", Fetch "x",Tru]
    let received = show (run (instructions, createEmptyStack, createEmptyState))
    let expected = ("Run-time error","")
    let passed = show (testAssembler instructions == expected)
    test "Test 11" received (show expected) passed

-- Part 2
data Aexp
  = Num Int          -- Represents a numeric constant
  | Var String       -- Represents a variable
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

-- -- TODO: Define the types Aexp, Bexp, Stm and Program

lexer :: String -> [String]
lexer = words

type Program = [Stm]

-- compA :: Aexp -> Code
-- compA = undefined -- TODO

-- -- compB :: Bexp -> Code
-- compB = undefined -- TODO

-- -- compile :: Program -> Code
-- compile = undefined -- TODO

-- -- parse :: String -> Program
-- parse = undefined -- TODO

-- -- To help you test your parser
-- testParser :: String -> (String, String)
-- testParser programCode = (stack2Str stack, store2Str store)
--   where (_,stack,store) = run(compile (parse programCode), createEmptyStack, createEmptyStore)

-- -- Examples:
-- -- testParser "x := 5; x := x - 1;" == ("","x=4")
-- -- testParser "x := 0 - 2;" == ("","x=-2")
-- -- testParser "if (not True and 2 <= 5 = 3 == 4) then x :=1; else y := 2;" == ("","y=2")
-- -- testParser "x := 42; if x <= 43 then x := 1; else (x := 33; x := x+1;);" == ("","x=1")
-- -- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1;" == ("","x=2")
-- -- testParser "x := 42; if x <= 43 then x := 1; else x := 33; x := x+1; z := x+x;" == ("","x=2,z=4")
-- -- testParser "x := 44; if x <= 43 then x := 1; else (x := 33; x := x+1;); y := x*2;" == ("","x=34,y=68")
-- -- testParser "x := 42; if x <= 43 then (x := 33; x := x+1;) else x := 1;" == ("","x=34")
-- -- testParser "if (1 == 0+1 = 2+1 == 3) then x := 1; else x := 2;" == ("","x=1")
-- -- testParser "if (1 == 0+1 = (2+1 == 4)) then x := 1; else x := 2;" == ("","x=2")
-- -- testParser "x := 2; y := (x - 3)*(4 + 2*3); z := x +x*(2);" == ("","x=2,y=-10,z=6")
-- -- testParser "i := 10; fact := 1; while (not(i == 1)) do (fact := fact * i; i := i - 1;);" == ("","fact=3628800,i=1")