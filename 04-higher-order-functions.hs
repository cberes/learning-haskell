-- Higher-order functions

-- Infix functions can be partially applied
(+3)
(/10)
(++ " HAHA")
("HAHA " ++)
(3:) -- so the other argument would need to be a list

-- lambdas
\x y -> x + y ^ 2 -- usually placed in parentheses, but not necessary
\(x,y) -> x + y ^ 2 -- support pattern matching as well

-- functions
map
filter
foldl
foldr  -- foldr works with infinite lists
foldl1 -- like fold left but uses the first value as the accumulator
foldr1 -- so you cannot use these with empty lists
foldl' -- strict version of foldl (values are stored rather than chains of functions to get those values)
foldr'
scanl  -- like foldl but returns a list of intermediate values (final value is last)
scanr  -- final value is first

-- function application operator

-- function calls have very high precedence, except the $
-- use it to evaluate expressions to the right before calling the function to the left
sum $ map sqrt [1..100] -- equivalent to sum (map sqrt [1..100])
sqrt $ 3 + 4 + 9 -- equivalent to sqrt (3 + 4 + 9)
sum $ filter (> 10) $ map (* 2) [2..10] -- sum (filter (> 10) (map (* 2) [2..10]))

-- also can apply an argument to list of partially-applied functions
map ($ 3) [(4+), (10*), (^2), sqrt]

-- function composition

f = (3+)
g = (9*)
f . g     -- same as f (g x)
f . g $ 2 -- same as f (g 2)

-- can use partially-applied functions, too
sum . replicate 4 $ max 6.7 8.9 -- 8.9 + 8.9 + 8.9 + 8.9

