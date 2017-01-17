-- modules

import Data.List
import Data.Graph (Edge, Vertex)
import Data.Array.IArray hiding (assocs)
import qualified Data.Map as M

digitSum :: Int -> Int
digitSum = sum . map digitToInt . show
head $ filter (\i -> digitSum i == 40) [1..]
find (\i -> digitSum i == 40) [1..] -- returns a Maybe Int

-- creating new modules

module Example.NewModule -- should be in Example/NewModule.hs
( firstTo -- list functions to export (these will be "public")
, findKey
, findKey'
) where   -- now define the functions

-- Maybe

Just "foo" -- a Maybe with a value
Nothing    -- a Maybe without a value

firstTo :: Int -> Maybe Int
firstTo n = find (\i -> digitSum i == n) [1..]

-- mapping keys to values

[("winnie", "pomeranian"), ("wendy", "lab"), ("maggie", "irish setter")] -- list of pairs

-- find key in a list of pairs
findKey :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey key [] = Nothing
findKey key ((k,v):xs)
    | key == k  = Just v
    | otherwise = findKey key xs

-- using fold
findKey' :: (Eq k) => k -> [(k, v)] -> Maybe v
findKey' key xs = foldr (\(k, v) acc -> if key == k then Just v else acc) Nothing xs

-- ^^^ this is Data.List.lookup

-- Data.Map

import qualified Data.Map as Map

dogs = Map.fromList [("winnie", "pomeranian"), ("wendy", "lab"), ("maggie", "irish setter")]
Map.lookup "winnie" dogs -- Just the value or Nothing
Map.size dogs -- number of entries
Map.insert "new-key" "new-value" dogs -- returns a new map with the new entry

import Data.Text (toUpper)
Map.map toUpper dogs -- returns a new map with keys altered by the function
                     -- the function can change the type of the map, too

-- create a map with a merge functions
Map.fromListWith (\breeds newBreed -> breeds ++ ", " ++ newBreeds) [("winnie", "pom"), ("wendy", "lab"), ("wendy", "pit bull")]

