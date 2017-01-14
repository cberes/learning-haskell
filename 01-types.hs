-- Types

-- Int
-- Integer: like Int, but unbounded
-- Float
-- Double
-- Bool
-- Char
-- (Int, Char): this is a tuple

-- Functions

factorial :: Integer -> Integer
factorial n = product [1..n]

-- Haskell has type variables (like generics)
-- head :: [a] -> a

-- Type classes

-- (==) :: (Eq a) => a -> a -> Bool
-- (Eq a) is a class constraint
-- it means that arguments must be instances of the Eq class

-- examples:
-- Eq: used by == and /=
-- Ord: used by <, >, <=, and >=
--   something that can be orders
--   see the compare function (it returns an Ordering [any of GT, LT, and EQ])
5 `compare` 3 -- GT
-- Show: instance can be represented as a string
show 3 -- "3"
-- Read: instance can be created from a string
read "[1,2,3]" ++ [4]
read "5" :: Int -- read "5" alone will not work, because type is ambiguous
[read "1",2,3]  -- this also works (but a tuple would not work)
-- Enum: sequentially ordered types (can be used in ranges)
succ 5   -- 6
pred 'b' -- 'a'
['A'..'Z']
-- Bounded: have upper and lower bounds
minBound :: Int
maxBound :: (Bool, Int, Char)
-- Num: numbers (arithmetic stuff)
-- Floating: floating-point numbers (cos, sqrt, etc.) 
-- Integral: like Num, but only whole numbers (Int and Integer)
fromIntegral (length [1,2,3,4]) -- returns a Num from an Int

