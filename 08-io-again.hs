-- I/O (again)

import Data.Char
uppercase = do
    contents <- getContents       -- reads all input from stdin as a single string
    putStr $ map toUpper contents

-- interact: reads input, applies a function, prints output
-- function is String -> String
shortLines = interact shortLinesOnly

shortLinesOnly :: String -> String
shortLinesOnly = unlines . filter (\line -> length line < 10) . lines

-- files

-- read from a file
import System.IO
printToStdin = do
    handle <- openFile "example.txt" ReadMode -- also: WriteMode, AppendMode, ReadWriteMode
    contents <- hGetContents handle
    putStr contents
    hClose handle

printToStdin' = do
    withFile "example.txt" ReadMode (\handle -> do -- accepts a function, closes the file for you
        contents <- hGetContents handle
        putStr contents)

-- bracket
withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' name mode f = bracket (openFile name mode) -- accepts an IO action,
    (\handle -> hClose handle)                       -- a function to close the handle
    (\handle -> f handle)                            -- and a function to use the handle

-- bracketOnError is an option
-- it accepts an IO action, a function to use if there is an error, and a function to use otherwise

-- like hGetContents and getContents,
-- there are hGetLine, hPutStr, hPutStrLn, and hGetChar as well

-- also functions to modify a file in one shot
readExample = do
    contents <- readFile "example.txt"
    putStr contents

writeExample = do
    contents <- readFile "example.txt"
    writeFile "example-upper.txt" (map toUpper contents) -- also: appendFile

-- command-line arguments
import System.Environment
main = do
    args <- getArgs         -- returns IO [String]
    progName <- getProgName -- returns IO  String

-- random values

import System.Random

random (mkStdGen 100) :: (Int, StdGen)  -- returns a random value and a new RandomGen
random (mkStdGen 100) :: (Bool, StdGen) -- also can use Float, Integer, etc.

threeCoins :: StdGen -> (Bool, Bool, Bool)
threeCoins gen =
    let (firstCoin, newGen) = random gen       -- use initial RandomGen
        (secondCoin, newGen') = random newGen  -- use RandomGen from first invocation
        (thirdCoin, newGen'') = random newGen'
    in (firstCoin, secondCoin, thirdCoin)

threeCoins (mkStdGen 21) -- invoke like this

-- could have used randoms (generates an infinite sequence of random values)
take 3 $ randoms (mkStdGen 21) :: [Bool]

randoms' :: (RandomGen g, Random a) => g -> [a]
randoms' gen =
    let (value, newGen) = random gen -- first, generate a new value
    in  value:randoms' newGen         -- then return value and recursively generate another value

finiteRandoms :: (RandomGen g, Random a) => Int -> g -> ([a], g)
finiteRandoms 0 gen = ([], gen)
finiteRandoms n gen =
    let (value, newGen) = random gen
        (restOfList, finalGen) = finiteRandoms (n - 1) newGen
    in  (value:restOfList, finalGen)

randomR (1, 6) (mkStdGen 1000)                        -- generate random in a range (inclusive)
take 10 $ randomRs ('a', 'z') (mkStdGen 10) :: [Char] -- generate sequence of rnadoms in a range

stdGenExample = do
    gen <- getStdGen -- gets a gobal RandomGen; returns IO StdGen
    putStrLn $ take 20 (randomRs ('a', 'z') gen)
    gen' <- newStdGen -- makes a new global RandomGen
    putStr $ take 20 (randomRs ('a', 'z') gen')

-- bytestrings: lists for file contents?

-- can be strict (Data.ByteString) or lazy (Data.ByteString.Lazy)
-- stored in 64-KB chunks (so 64 KB will be read at a time)
-- lazy can be slower

import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as S

-- pack is a function of [Word8] -> ByteString
-- Word8 is an unsigned 8-bit number
-- (larger numbers overflow and restart at 0)
B.pack [99,97,110]
B.pack [98..120]
B.unpack $ B.pack [98,111,114,116] -- returns a [Word8]

-- convert a list of strict bytestrings to one lazy bytestring
B.fromChunks [S.pack [40,41,42], S.pack [43,44,45], S.pack[46,47,48]]

B.cons 85 $ B.pack [80,81,82,84] -- like : operator

-- see also: head, tail, init, null, length, map, reverse, foldl, foldr, concat, takeWhile, filter
-- functions for files, too: readFile :: FilePath -> IO ByteString

