module Exercise where

import Prelude
import Partial.Unsafe (unsafePartial)
import Control.MonadZero (guard)
import Data.Array (filter, null, snoc, (..), (:))
import Data.Array.Partial (head, tail)
import Data.Foldable (foldl, foldr)

length :: forall a. Array a -> Int
length arr =
  if null arr
    then 0
    else 1 + length (unsafePartial tail arr)

-- Write a recursive function which returns true if and
-- only if its input is an even integer
even :: Int -> Boolean
even n =
  if n < 0
  then even (-n)
  else if n == 0
     then true
     else if n == 1
        then false
        else even (n-2)

-- Write a recursive function which counts the number of even integers in an array
countEvens :: Array Int -> Int
countEvens xs =
  if null xs
  then 0
  else if even $ unsafePartial $ head xs
       then 1 + (countEvens $ unsafePartial $ tail xs)
       else countEvens $ unsafePartial tail xs

doubleNumbers :: Array Int -> Array Int
-- doubleNumbers = map (\n -> n + 1)
doubleNumbers xs = (\n -> n + 1) <$> xs

-- Use the map or <$> function to write a function which calculates
-- the squares of an array of numbers
squareAll :: Array Int -> Array Int
squareAll = map (\n -> n * n)

-- Use the filter function to write a function which removes the
-- negative numbers from an array of numbers
removeNegatives :: Array Int -> Array Int
removeNegatives = filter (\n -> n >= 0)

-- Define an infix synonym <$?> for filter. Rewrite your answer to
-- the previous question to use your new operator
infix 8 filter as <$?>
removeNegatives_ :: Array Int -> Array Int
removeNegatives_ xs = (\n -> n >= 0) <$?> xs

factors :: Int -> Array (Array Int)
factors n = do
  i <- 1 .. n
  j <- i .. n
  guard $ i * j == n
  pure [i, j]

-- Use the factors function to define a function isPrime which tests if
-- its integer argument is prime or not
isPrime :: Int -> Boolean
isPrime = ((==) 1) <<< length <<< factors

-- Write a function which uses do notation to find the cartesian product
-- of two arrays, i.e. the set of all pairs of elements a, b, where a is an
-- element of the first array, and b is an element of the second
xProd :: Array Int -> Array Int -> Array (Array Int)
xProd xs ys = do
  x <- xs
  y <- ys
  pure [x, y]

-- A Pythagorean triple is an array of numbers [a, b, c] such that
-- a² + b² = c². Use the guard function in an array comprehension to
-- write a function triples which takes a number n and calculates all
-- Pythagorean triples whose components are less than n
triples :: Int -> Array (Array Int)
triples n = do
  a <- 1..m
  b <- 1..m
  c <- 1..m

  guard $ a * a + b * b == c * c
  pure [a, b, c]
  where m = n - 1

-- TODO: Write a function factorizations which produces all factorizations of
-- an integer n, i.e. arrays of integers whose product is n.


-- this will build up a big stack
fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

-- tail call recursive
fact' :: Int -> Int -> Int
fact' 0 acc = acc
fact' n acc = fact' (n - 1) (acc * n)

-- prefer folds to explicit recursion
fact'' :: Int -> Int
fact'' n = foldr (*) 1 (1..n)

reverse :: forall a. Array a -> Array a
reverse = foldr (\x xs -> xs <> [x]) []

reverse' :: forall a. Array a -> Array a
reverse' = foldr (:) []

-- Write reverse in terms of foldl.
-- oops, already did it with foldr
reverse'' :: forall a. Array a -> Array a
reverse'' = foldl snoc []

-- Use foldl to test whether an array of boolean values are all true
all :: Array Boolean -> Boolean
all = foldr (&&) true
