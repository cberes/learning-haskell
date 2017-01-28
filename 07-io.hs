-- Input and Output

-- functions that receive input or produce output return I/O actions
-- actions have side effects (input or output)
-- reading input: return type is IO a (eg. IO String)
name <- getLine -- bind operator?
-- writing output: return type is IO ()
-- the () in IO () is an empty tuple
putStrLn $ "Your name is " ++ name

-- can put multiple I/O actions together in a do block
main = do
    putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn ("Hey " ++ name ++ ", you rock!")

-- to take data out of an I/O action, you must be in an I/O action

-- can't do something like
-- "some text now: " ++ getLine
-- because types are String and IO String

-- in I/O actions, use the return keyword to make an I/O action from a pure value
stupidRead = do
  line <- getLine
  return line

-- it does not exit the function if there are more statements
foo = do
  a <- return "foo" -- but this is stupid, because you can use let
  b <- return "bar"
  putStrLn $ foo ++ " " ++ bar

-- more functions
putStr "foo" -- like putStrLn but with no following new line
putChar 'a'  -- like putStr but for a single character
print True   -- outputs anything that implements Show (but puts quotes around strings)

-- when
-- given a boolean and an I/O actions, return either the I/O action or an IO ()
import Control.Monad
fish = do
    input <- getLine
    when (input == "SWORDFISH") $ do
        putStrLn input

-- sequence
sequenceExample = do
    rs <- sequence [getLine, getLine, getLine] -- invokes getLine 3 times
    print rs
sequence $ map print [1, 2, 3, 4 5] -- prints the numbers on each line
-- and it returns an IO [(),(),(),(),()]

-- mapM
-- maps an function that returns an I/O action over a list and then sequences it
mapM print [1,2,3,4,5]  -- returns IO [(),(),(),(),()]
mapM_ print [1,2,3,4,5] -- no return value

-- see also forM in Control.Monad (which swaps arguments to mapM)

