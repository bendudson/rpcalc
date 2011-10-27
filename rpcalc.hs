-- A simple Reverse-Polish calculator in Haskell
-- B.Dudson 2010

import Prelude hiding ( catch )
import Control.Exception
import System.IO

--import IO
--import Control.Monad.Error
--import Control.Exception

-- Test if a char is present in a string
inStr :: String -> Char -> Bool
inStr [] _ = False
inStr (x:xs) c = if x == c then True else inStr xs c

-- Test if a string is a number
isNum :: String -> Bool
isNum s = 
  let
    -- Test if a char is legal in a number
    charIsLegal :: Char -> Bool
    charIsLegal = inStr "0123456789-.eE"
    strIsLegal xs = and (map charIsLegal xs)  
    -- Test if a char is a number
    charIsNum :: Char -> Bool
    charIsNum = inStr "0123456789"
    strHasNum xs = or (map charIsNum s)
  in
    (strIsLegal s) && (strHasNum s) -- Legal string, and contains a number

-- A list of commands. In general a better way would be lookup tables,
-- but this is simple, and quick for a small number of commands
cmdList :: [([Char], (Float -> Float -> Float))]
cmdList = [("+", \x y -> x + y),
           ("-", \x y -> x - y),
           ("*", \x y -> x * y),
           ("/", \x y -> x / y),
           ("^", \x y -> x ** y),
           ("atan2", \x y -> atan2 x y)]

-- List of single-argument commands
opList :: [(String , (Float -> Float))]
opList = [("sin", \x -> sin x),
          ("cos", \x -> cos x),
          ("tan", \x -> tan x),
          ("asin", \x -> asin x),
          ("acos", \x -> acos x),
          ("atan", \x -> atan x),
          ("exp" , \x -> exp x),
          ("log" , \x -> log x)]

-- List of values
valList :: [(String, Float)]
valList = [("pi", pi),
           ("e", exp 1)]

-- Search a list of commands 
findCmd :: [(String, a)] -> String -> Maybe a
findCmd [] str = Nothing
findCmd (c:cs) str = if (fst c) == str then Just (snd c) else findCmd cs str

-- Run a command 
runCmd :: String -> [Float] -> [Float]
runCmd [] xs = xs      -- Null command
runCmd str (x:y:xs) =  -- If there are > two elements in the list
    case findCmd cmdList str of
      Nothing -> case findCmd opList str of -- Test for a single-arg command
        Nothing -> error "No such command"
        Just f  -> (f x) : y : xs
      Just f -> (f y x) : xs
runCmd str (x:xs) =    -- If only one element in list
    case findCmd opList str of
      Nothing -> case findCmd cmdList str of
        Nothing -> error "No such command"
        Just f -> error "Not enough parameters"
      Just f -> (f x) : xs
runCmd _ xs = error "Not enough parameters"

-- Print the stack

printStack :: [Float] -> IO ()
printStack [] = putStr ""
printStack (x:xs) = do
  putStrLn (show x)
  printStack xs

-- Print help
printHelp :: IO ()
printHelp =
  let
    listOps :: [(String, a)] -> String
    listOps [] = []
    listOps (x:[]) = fst x
    listOps (x:xs) = (fst x) ++ ", " ++ (listOps xs)
  in do
    putStr ("\nCommands:\n"
        ++ "   q     Quit\n"
        ++ "   ?     Print this help\n"
        ++ "   d     Delete the first number\n"
        ++ "   r     Reverse the first two numbers (below single dash)\n"
        ++ "   R     Reverse entire stack\n\n"
        ++ "Numbers operated on from bottom to top. Those below single\n"
        ++ "dashed line will be operated on next.\n\n"
        ++ "Values: " ++ (listOps valList) ++ "\n"
        ++ "Single-argument functions: " ++ (listOps opList) ++ "\n"
        ++ "Two-argument functions: " ++ (listOps cmdList) ++ "\n"
        )

-- This gets a string from the user, ignoring empty lines
getInput :: IO String
getInput = do
  putStr "> "
  hFlush stdout -- Need to flush output to get prompt
  str <- getLine
  if str == []
    then getInput
    else return str

-- Main loop of Reverse Polish calculator
runCalc :: [Float] -> IO ()
runCalc xs = 
  let
    useStr :: [Float] -> String -> IO ()
    useStr xs str
      | (head str) == 'q' -- Check for a quit command
        = putStrLn "Exiting"
      | (head str) == '?' -- Help
        = do
            printHelp
            runCalc xs
      | str == "d"  -- Delete the first element
        = runCalc ( drop 1 xs )
      | str == "r"  -- Reverse the top two in the stack
        = runCalc ( (reverse (take 2 xs)) ++ (drop 2 xs) )
      | str == "R"  -- Reverse entire stack
        = runCalc (reverse xs)
      | isNum str   -- If a number, put onto stack
        = let
            -- read can't handle leading '.' so fix first
            str2 = if head str == '.'
              then '0' : str
              else if (take 2 str) == "-."
                then "-0." ++ (drop 2 str)
                else str
          in
            catch (runCalc ((read str2) : xs)) (errHandler str)
      | otherwise
        = case findCmd valList str of -- Check for special values
            Nothing -> catch (runCalc (runCmd str xs)) (errHandler str)
            Just v -> runCalc (v : xs) -- Put value on stack
    errHandler :: String -> SomeException -> IO ()
    errHandler s err = do
      hPutStrLn stderr ("Error behind keyboard: '" ++ s ++ "' -> " ++ show err)
      runCalc xs
  in do
    -- Print the current stack
    putStrLn "\n\n==========="
    printStack (reverse (drop 2 xs))
    putStrLn "-----------"
    printStack (reverse (take 2 xs))
    -- Get the next input from user
    str <- getInput
    -- Decide what to do with it
    useStr xs str

main = do
  putStrLn "\nSimple Reverse Polish calculator"
  putStrLn "   B.Dudson, Dec 2008\n"
  putStrLn "Type '?' for help"
  runCalc []
