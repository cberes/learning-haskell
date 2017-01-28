-- Custom Types

data Bool' = True' | False' -- a boolean
data Int' = -6 | -5 | -4 | -3 | -2 | -1 | 0 | 1 | 2 | 3 | 4 | 5 | 6 -- an int with a really small range

modules Shapes -- export constructors and functions
( Point(..)    -- (..) exports all constructors
, Shape(..)    -- could also write Shape (Rectangle, Circle)
, area         -- or only Shape to export no constructors
) where

-- Circle and Rectangle are two functions that return a Shape
data Shape = Circle Float Float Float | Rectangle Float Float Float Float
    deriving (Show) -- lets you convert the class to a string

-- a function with a Shape parameter
area :: Shape -> Float
area (Circle _ _ r) = pi * r ^ 2 -- you can pattern-match on constructors
area (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)
-- can match nested constructors, too, if arguments are complex

area $ Circle 10 20 10 -- invoke like this

-- can have a single constructor
data Point = Point Float Float deriving (Show, Read)

-- Record syntax

data Person = Person { firstName :: String
                     , lastName :: String
					 , age :: Int
					 , height :: Float
					 , phoneNumber :: String
					 , flavor :: String }
					 deriving (Show)

-- record syntax creates accessors for each field
age $ Person "Corey" "Beres" 28 68 "555-1234" "vanilla"

-- can create instances like this
Person "Corey" "Beres" 28 68 "555-1234" "vanilla"
-- or like this
Person {firstName="Corey", lastName="Beres", age=28, height=68, phoneNumber="555-1234", flavor="vanilla"}

-- type parameters

data Maybe' a = Nothing' | Just' a
-- constraints for abstract types can be added to function definitions

data IntMaybe = INothing | IJust Int -- can make a concrete type this way

-- derived types

-- can derive other types like Eq the way we derived Show
-- to derive Eq, all of a type's fields must also derive Eq

-- can derive Read, then invoke like this
read "Point 1 2 3"
read "Just 3" :: Maybe Int

-- as with Eq: to derive Ord, all of a type's fields must also derive Ord
-- Values specified earlier in the data type definition are considered to be less than later values.

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday deriving (Eq, Ord, Show, Read, Bounded, Enum)
-- because all constructors do not have parameters, the type can inherit Enum
-- can inherit Bounded because there are least and greatest values
pred Saturday -- Enum
succ Saturday
[Thursday..Saturday]
[minBound..maxBound] :: [Day]
minBound :: Day -- Bounded
maxBound :: Day
Saturday == Saturday -- Eq
Saturday > Friday -- Ord

-- type synonyms

-- they cannot be invoked like constructors
-- but they can be used as types for functions
type Days = [Day]
type AssocList k v = [(k, v)] -- can have type parameters
type IntAssocList = AssocList Int -- can be partially applied

-- infix constructors

-- infix constructors must begin with a colon
infixr 5 :-: -- the fixity declaration
data List' a = Empty' | a :-: (List a) -- our own Cons (:) constructor
    deriving (Show, Read, Eq, Ord)

-- fixity declaration:
-- * can be either infixr (right associative) or infixl (left associative)
-- * the number describes how tightly the operator binds

-- functions can also have fixity declarations
infixr 5 ^++
(^++) :: List' a -> List' a -> List' a
Empty'   ^++ ys = ys
(x:-:xs) ^++ ys = x :-: (xs ^++ ys)

-- custom type classes

class Eq' a where
    (==) :: a -> a -> Bool -- type declarations (required)
    (/=) :: a -> a -> Bool
    x == y = not (x /= y)  -- default functions bodies
    x /= y = not (x == y)  -- because they refer to each other, we will need to implement at least one

data trafficLight = Red | Yellow | Green

-- an implementation of the type class
-- notice that /= does not need to be defined here
-- also, note that adding 'deriving (Eq)' to TrafficLight would have had the same effect
instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False

-- could have added 'deriving (Show)' but the resulting strings would have been "Red", "Green", etc.
instance Show TrafficLight where
    show Red = "Red light"
    show Yellow = "Yellow light"
    show Green = "Green light"

-- subclasses

class (Eq a) => Ord' a where -- Ord' is a subclass of Eq
    foo :: a -> a -> Int

-- a parameterized type
instance (Eq' m) => Eq' (Maybe m) where -- the parameter must be of type Eq
    Just x == Just y = x == y           -- because the values are compared with == here
    Nothing == Nothing = True
    _ == _ = False

