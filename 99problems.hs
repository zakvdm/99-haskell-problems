{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
import Test.Tasty
import Test.Tasty.SmallCheck
import Test.SmallCheck.Series
import Test.Tasty.HUnit
import Data.List
import Control.Monad

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
    testGroup "99Problems"
    [
          testProperty "1" prop_myLast 
--      , testProperty "2" prop_myLastButOne
--      , testProperty "3" prop_kth
--      , testProperty "4" prop_myLength
--      , group5
--      , group6
      , group7
      , group8
      , group9
      , group10
    ]

-- Problem 1
prop_myLast xs = length (xs :: [Int]) > 0 ==> myLast xs == last xs
myLast :: [a] -> a
myLast [x] = x
myLast (_:xs) = myLast xs

{-  2 Problem 2
      (*) Find the last but one element of a list.
    Example in Haskell:
      Prelude> myButLast [1,2,3,4]
      3
      Prelude> myButLast ['a'..'z']
     'y'
-}
prop_myLastButOne :: Monad m => [Int] -> Property m
prop_myLastButOne xs = length xs > 1 ==> myLastButOne xs == (reverse xs) !! 1

myLastButOne :: [a] -> a
myLastButOne (x:xs)
  | length xs == 1 = x
  | otherwise = myLastButOne xs

{- 3 Problem 3
     (*) Find the K'th element of a list. The first element in the list is number 1.
   Example:
     * (element-at '(a b c d e) 3)
     c
   Example in Haskell:
     Prelude> elementAt [1,2,3] 2
     2
     Prelude> elementAt "haskell" 5
     'e'
-}
prop_kth xs x = x >= 0 && length (xs :: [Int]) > 0 ==> let k = mod x (length xs) in kth xs k == xs !! k

kth :: (Ord b, Num b) => [a] -> b -> a
kth xs i
  | i <= 0 = head xs
  | otherwise = kth (tail xs) (i - 1)

{- 4 Problem 4

  (*) Find the number of elements of a list.

  Example in Haskell:

  Prelude> myLength [123, 456, 789]
  3
  Prelude> myLength "Hello, world!"
  13
-}

prop_myLength :: [Int] -> Bool
prop_myLength xs = myLength xs == length xs

myLength :: [a] -> Int
myLength [] = 0
myLength [_] = 1
myLength (_:xs) = 1 + (myLength xs)

{-
- (*) Reverse a list.
-
- Example in Haskell:
-
- Prelude> myReverse "A man, a plan, a canal, panama!"
- "!amanap ,lanac a ,nalp a ,nam A"
- Prelude> myReverse [1,2,3,4]
- [4,3,2,1]
-}
group5 = testGroup "5" [
    testProperty "symmetric" $
      \xs -> (xs :: [Int]) == (myReverse (myReverse xs))
    , testProperty "reversed" $
      \xs -> myReverse (xs :: [Int]) == reverse xs
  ]

myReverse :: [a] -> [a]
myReverse [] = []
myReverse [x] = [x]
myReverse (x:xs) = (myReverse xs) ++ [x]

{-
 6 Problem 6
(*) Find out whether a list is a palindrome. A palindrome can be read forward or backward; e.g. (x a m a x).

Example in Haskell:
*Main> isPalindrome [1,2,3]
False
*Main> isPalindrome "madamimadam"
True
*Main> isPalindrome [1,2,4,8,16,8,4,2,1]
True
-}
palindromes :: String -> Char -> [String]
palindromes xs s = [even, odd]
  where even = xs ++ (reverse xs)
        odd = xs ++ [s] ++ (reverse xs)



group6 = testGroup "6" [
    testProperty "positive" $
      \xs s -> all isPalindrome (palindromes xs s)
  ]

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = reverse xs == xs


-- TODO: WTF how do i generate a tree? How can I test that this instance of
-- Serial is correct???
data Tree a = Null | Fork (Tree a) a (Tree a)

instance Serial m a => Serial m (Tree a) where
      series = cons0 Null \/ cons3 Fork

{-
(**) Flatten a nested list structure.

Transform a list, possibly holding lists as elements into a `flat' list by replacing each list with its elements (recursively).

Example:

* (my-flatten '(a (b (c d) e)))
(A B C D E)

Example in Haskell:

We have to define a new data type, because lists in Haskell are homogeneous.

 data NestedList a = Elem a | List [NestedList a]

*Main> flatten (Elem 5)
[5]
*Main> flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
[1,2,3,4,5]
*Main> flatten (List [])
[]
-}

group7 = testGroup "7 - flatten" [
    testCase "empty list" $ flatten ((List []) :: NestedList Int) @=? []
    , testCase "single element list" $ flatten (Elem 5) @=? [5]
    , testCase "nested list - simple" $ flatten (List [Elem 1]) @=? [1]
    , testCase "nested list - complex" $
        flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]) @=? [1, 2, 3, 4, 5]
  ]

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List xs) = xs >>= flatten

{-
Problem 8

(**) Eliminate consecutive duplicates of list elements.

If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.

Example:

* (compress '(a a a a b c c a a d e e e e))
(A B C A D E)

Example in Haskell:

> compress "aaaabccaadeeee"
"abcade"
-}
group8 = testGroup "compress" [
  testCase "empty list" $ compress "" @=? []
  , testCase "string" $ compress "aaaabccaadeeee" @=? "abcade"
  ]

compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:xs)
  | x == head xs = compress xs
  | otherwise = x : compress xs


{-
 9 Problem 9

(**) Pack consecutive duplicates of list elements into sublists. If a list contains repeated elements they should be placed in separate sublists.

Example:

* (pack '(a a a a b c c a a d e e e e))
((A A A A) (B) (C C) (A A) (D) (E E E E))

Example in Haskell:

*Main> pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 
             'a', 'd', 'e', 'e', 'e', 'e']
             ["aaaa","b","cc","aa","d","eeee"]
-}


group9 = testGroup "pack duplicates" [
    testCase "complex case" $
      pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e'] @=? ["aaaa","b","cc","aa","d","eeee"]
  ]

pack :: Eq a => [a] -> [[a]]
pack [] = []
pack [a] = [a]
pack (x:xs)
  | x == head xs = 


group10 = testGroup "10" []






