import Data.List (sort)

type Stack = [String]

createEmptyStack :: Stack
createEmptyStack = []

stack2Str :: Stack -> String
stack2Str stack =
  let myList = concatMap showValue stack
  in if null myList then [] else init myList
  where showValue var = var ++ ","

printStack :: Stack -> String -> IO ()
printStack stack expected = putStrLn (stack2Str stack ++ if stack2Str stack == expected then " Test Passed" else " Test Failed")

type State = [(String, String)]

createEmptyState :: State
createEmptyState = []

state2Str :: State -> String
state2Str state = 
  let myList = concatMap showPair $ sort state
  in if null myList then [] else init myList
  where showPair (var, value) = var ++ "=" ++ value ++ ","

printState :: State -> String -> IO ()
printState state expected = putStrLn (state2Str state ++ if state2Str state == expected then " Test Passed" else " Test Failed")

-- Example usage:
main :: IO ()
main = do
    let myStack = ["-20","True","False"]
    let expectedStack = "False,True,-20"
    let myState = [("z", "10"), ("y", "20"), ("x", "30")]
    let expectedState = "x=30,y=20,z=10"
    printStack myStack expectedStack
    printState myState expectedState
