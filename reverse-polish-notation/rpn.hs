import System.Environment
import System.IO

main = do
    args <- getArgs
    let result = head $ doRpn [] args
    putStrLn $ show result

doRpn :: [Int] -> [String] -> [Int]
doRpn stack [] = stack
doRpn stack (x:xs) = doRpn (evaluate stack x) xs

evaluate :: [Int] -> String -> [Int]
evaluate (a:b:xs) "+" = b + a : xs
evaluate (a:b:xs) "-" = b - a : xs
evaluate (a:b:xs) "*" = b * a : xs
evaluate (a:b:xs) "/" = b `div` a : xs
evaluate stack number = read number : stack

