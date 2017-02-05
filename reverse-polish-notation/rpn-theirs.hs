import System.Environment
import System.IO

main = do
    args <- getArgs
    let result = solveRpn $ head args
    putStrLn $ show result

solveRpn :: String -> Double
solveRpn = head . foldl foldingFunc [] . words
    where foldingFunc (a:b:bs) "+" = b + a : bs
          foldingFunc (a:b:bs) "-" = b - a : bs
          foldingFunc (a:b:bs) "*" = b * a : bs
          foldingFunc (a:b:bs) "/" = b / a : bs
          foldingFunc (b:bs) "ln" = log b : bs
          foldingFunc bs "sum" = [sum bs]
          foldingFunc bs num = read num : bs

