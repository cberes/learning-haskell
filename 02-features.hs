-- Pattern matching

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Brina"
-- calling charName 'c' will cause an error, because no case matches that pattern

-- with tuples
addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

-- can use _ as wildcard in function parameters
first :: (a, b, c) -> a
first (x, _, _) = x

-- with lists
head' :: [a] -> a
head' [] = error "empty list"
head' (x:_) = x

foo :: (Show a) => [a] -> String
foo [] = "empty list"
foo (x:[]) = "one element: " ++ show x
foo (x:y:[]) = "two elements: " ++ show x ++ " and " ++ show y
foo (x:y:_) = "first two elements: " ++ show x ++ " and " ++ show y

-- as-pattern
foo' :: (Show a) => [a] -> String
foo' [] = "empty list"
foo' (x:[]) = "one element: " ++ show x
foo' all@(x:_) = "first element of " ++ show (length all) ++ " is " ++ show x

-- guards

gradeTell :: Int -> String
gradeTell score
    | score >= 100 = "Perfect! You're a star!"
    | score >= 90  = "Great Job! A"
    | score >= 80  = "Nice! B"
    | score >= 70  = "Cs get degrees!"
    | score >= 60  = "D"
    | otherwise    = "Loser! You're a loser!"

max' :: (Ord a) => a -> a -> a
max' a b
    | a <= b    = b
    | otherwise = a

compare' :: (Ord a) => a -> a -> Ordering
a `compare'` b
    | a == b    = EQ
    | a < b     = LT
    | otherwise = GT

-- can fall through to another definition
take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

-- where

bmiTell :: Double -> Double -> String
bmiTell weight height
    | bmi <= 18.5 = "underweight"
    | bmi <= 25.0 = "normal"
    | bmi <= 30.0 = "overweight"
    | otherwise   = "obese"
    where bmi = weight / height ^ 2 -- can place multiple definitions here
                                    -- one per line, all starting at the same column

-- can pattern match in where
gradeTell' :: Int -> String
gradeTell' score
    | score >= A = "You got an A"
    | score >= B = "You got a B"
    | score >= C = "Cs get degrees"
    | otherwise  = "Better luck next time"
    where (A, B, C) = (90, 80, 70)

initials :: String -> String -> String
initials firstName lastName = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstName
          (l:_) = lastName

-- functions can be declared in where block
calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi w h | (w, h) <- xs]
    where bmi weight height = weight / height ^ 2

-- let expressions

-- they are like where bindings, but they return a value
-- and let expressions cannot be used in guards
cylinder :: Double -> Double -> Double
cylinder radius height
    let sideArea = 2 * pi * radius * height
        topArea = pi * radius ^ 2
    in  sideArea + 2 * topArea

-- can define functions
[let square x = x * x in (square 3, square 4, square 5)]

-- can put definitions on one line
(let a = 100; b = 200; c = 300 in a * b * c, let foo = "Foo "; bar = "bar." in foo ++ bar)

-- and allow pattern matching
(let (a, b, c) = (1, 2, 3) in a + b + c) * 100

-- in list comprehensions
calcBmis' :: [(Double, Double)] -> [Double]
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2] -- put filters last
                      -- this part is the generator
                      -- it's defined before the let expression

-- case expressions

describeList :: [a] -> String
describeList ls = "The list is " ++ case ls of [] -> "empty."
                                               [x] -> "a singleton list."
                                               xs -> "a longer list."

-- could also have used a where block
describeList' :: [a] -> String
describeList' ls = "The list is " ++ what ls
    where what [] -> "empty."
          what [x] -> "a singleton list."
          what xs -> "a longer list."

