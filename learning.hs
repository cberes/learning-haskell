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

