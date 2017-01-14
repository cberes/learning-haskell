-- Haskell

-- numbers

2 + 15
49 * 100
1892 - 1472
5 / 2

-- booleans

True && False
True && True
False || True

-- equality

5 == 5
1 == 0
5 /= 5
5 /= 4
"hello" == "hello" -- True

-- functions

succ 8      -- gets the next thing (the successor)
min 9 10    -- minimum
min 3.4 3.2
max 100 101
-- functions have the highest precedence
succ 9 + 10 -- 20
-- use backticks to call a prefix function as if it were infix
92 `div` 10 -- 9

-- declaring functions

doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = if x > 100
                        then x
                        else x * 2

-- lists

numbers = [1, 2, 3, 5, 8, 13] -- use let to declare this in GHCi
['h', 'e', 'l'] ++ ['l', 'o'] -- merges the two lists
-- merging is a linear operation
-- prepending is constant, see cons operator
1 : [2, 3, 4, 5]
'l' : "lama"

-- list access

"Winnie" !! 1 -- i
[5, 2, 8, 9, 10] !! 2 -- 8
head [5, 2, 8, 9, 10] -- the first element
tail [5, 2, 8, 9, 10] -- everything but the first element
last [5, 2, 8, 9, 10] -- the last element
init [5, 2, 8, 9, 10] -- everything but the last element
take 2 [5, 2, 8, 9, 10] -- [5, 2]
drop 2 [5, 2, 8, 9, 10] -- [8, 9, 10]

--list comparison

[1,2,4] >  [1,2,3]
[1,2,4] >= [1,2,3]
[1,2,3] == [1,2,3]
[10] > [1,2]    -- comparisons are done element-wise
[1,2] < [1,2,0] -- the lack of an element is less than any element

-- more list functions

length [1,2,3]
null [] -- checks for empty list
reverse [3,2,1]
maximum [1,2,3]
minimum [1,2,3]
sum [2,3,4]     -- 9
product [2,3,4] -- 24
elem 4 [2,3,4]   -- check if in list
4 `elem` [2,3,4] -- usually called as infix

-- ranges

[1..20]    -- first and last elements inclusive
['a'..'z']
[2,4..20]  -- specify first 2 elements to determine the step
[1..]      -- omit the final element to make an infinite list

take 10 (cycle [1,2,3]) -- [1,2,3,1,2,3,1,2,3,1]
take 10 (repeat 5)      -- [5,5,5,5,5,5,5,5,5,5]
replicate 10 5          -- [5,5,5,5,5,5,5,5,5,5]

-- list comprehensions

[x * 2 | x <- [1..10]] -- multiply by 2 every number 1-10
[x * 2 | x <- [1..10], x * 2 >= 12] -- same as above but keep only elements greater than 12
                                    -- can have multiple filters separated by commas
boomBangs xs = [ if x < 10 then "BOOM!" else "BANG!" | x <- xs, odd x]
[x + y | x <- [1,2,3], y <- [10,100,1000]] -- multiple predicates ... yields 9 elements
length' xs = sum [1 | _ <- xs] -- use _ if you don't care about the value
-- comprehensions can be nested, too

-- tuples

-- fixed size (>= 2), possibly different types
(3, 'a', "hello")
-- number of elements in a tuple is part of its type
-- so this is invalid: [(1,2), (1,2,3)]
-- same with the types of the elements
-- this is invalid [(1,2), (1,"two")]

fst (8, 11) -- 8    these are available only for pairs`
snd (8, 11) -- 11

zip [1,3,5] [2,6,8] -- [(1,2), (3,4), (5,6)]
-- can zip lists of different sizes
-- but cannot compare tuples of different sizes

-- tuples of sides (length <= 10) of right triangles with perimeter == 24
[(a,b,c) | c <- [1..10], a <- [1..c], b <- [1..a], c ^ 2 == a ^ 2 + b ^ 2, a + b + c == 24]

