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
--          testProperty "1" prop_myLast
--      , testProperty "2" prop_myLastButOne
--      , testProperty "3" prop_kth
--      , testProperty "4" prop_myLength
--      , group5
--      , group6
--      , group7
--      , group8
--      , group9
--      , group10
--      ,
        group11
        , group12
        , group13
        , group14
        , group15
        , group16
        , group17
        , group18
        , group19
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
prop_myLastButOne xs = length xs > 1 ==> myLastButOne xs == reverse xs !! 1

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
myLength (_:xs) = 1 + myLength xs

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
      \xs -> (xs :: [Int]) == (myReverse $ myReverse xs)
    , testProperty "reversed" $
      \xs -> myReverse (xs :: [Int]) == reverse xs
  ]

myReverse :: [a] -> [a]
myReverse [] = []
myReverse [x] = [x]
myReverse (x:xs) = myReverse xs ++ [x]

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
    testCase "empty list" $ flatten (List [] :: NestedList Int) @=? []
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


group9 :: TestTree
group9 = testGroup "pack duplicates" [
    testCase "complex case" $
       ["aaaa","b","cc","aa","d","eeee"] @=? pack "aaaabccaadeeee"
  ]

pack :: Eq a => [a] -> [[a]]
pack = foldr takeNode []
  where takeNode x [] = [[x]]
        takeNode x (y:xs)
          | x == head y = (x : y) : xs
          | otherwise = [x] : y : xs

{-
Problem 10

(*) Run-length encoding of a list. Use the result of problem P09 to implement the so-called run-length encoding data compression method. Consecutive duplicates of elements are encoded as lists (N E) where N is the number of duplicates of the element E.

Example:

* (encode '(a a a a b c c a a d e e e e))
((4 A) (1 B) (2 C) (2 A) (1 D)(4 E))

Example in Haskell:

encode "aaaabccaadeeee"
[(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
-}
group10 :: TestTree
group10 = testGroup "run-length encoding" [
    testCase "empty string" $
      [] @=? encode ""
    , testCase "complex case" $
      [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')] @=? encode "aaaabccaadeeee"
  ]

encode :: Eq a => [a] -> [(Int,a)]
encode xs = map counter $ group xs
  where counter ys = (length ys, head ys)

{-
Problem 11

(*) Modified run-length encoding.

Modify the result of problem 10 in such a way that if an element has no duplicates it is simply copied into the result list. Only elements with duplicates are transferred as (N E) lists.

Example:

* (encode-modified '(a a a a b c c a a d e e e e))
((4 A) B (2 C) (2 A) D (4 E))

Example in Haskell:

P11> encodeModified "aaaabccaadeeee"
[Multiple 4 'a',Single 'b',Multiple 2 'c',
 Multiple 2 'a',Single 'd',Multiple 4 'e']
-}
group11 :: TestTree
group11 = testGroup "modified run-length encoding" [
    testCase "empty string" $
      [] @=? encodeModified ""
    , testCase "complex case" $
      [Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e'] @=? encodeModified "aaaabccaadeeee"
  ]

data Encoded a = Multiple Int a | Single a deriving (Show, Eq)

encodeModified :: Eq a => [a] -> [Encoded a]
encodeModified = map modify . encode
  where modify (1, v) = Single v
        modify (l, v) = Multiple l v

{-
 Problem 12

 (**) Decode a run-length encoded list.

 Given a run-length code list generated as specified in problem 11. Construct its uncompressed version.

 Example in Haskell:

 P12> decodeModified
        [Multiple 4 'a',Single 'b',Multiple 2 'c',
                Multiple 2 'a',Single 'd',Multiple 4 'e']
                "aaaabccaadeeee"
-}

group12 :: TestTree
group12 = testGroup "decode run-length encoded list" [
    testCase "empty string" $
      "" @=? decode []
    , testCase "complex case" $
      "aaaabccaadeeee" @=? decode [Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e']
  ]

decode :: [Encoded a] -> [a]
decode xs = xs >>= expand where
  expand (Single x) = [x]
  expand (Multiple n x) = replicate n x

{-
Problem 13

(**) Run-length encoding of a list (direct solution).

Implement the so-called run-length encoding data compression method directly. I.e. don't explicitly create the sublists containing the duplicates, as in problem 9, but only count them. As in problem P11, simplify the result list by replacing the singleton lists (1 X) by X.

Example:

* (encode-direct '(a a a a b c c a a d e e e e))
((4 A) B (2 C) (2 A) D (4 E))

Example in Haskell:

P13> encodeDirect "aaaabccaadeeee"
[Multiple 4 'a',Single 'b',Multiple 2 'c',
 Multiple 2 'a',Single 'd',Multiple 4 'e']
-}


group13 :: TestTree
group13 = testGroup "direct encoding of run-length encoded list" [
    testCase "empty string" $
      [] @=? encodeDirect ""
    , testCase "complex case" $
      [Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e'] @=? encodeDirect "aaaabccaadeeee"
  ]

encodeDirect :: Eq a => [a] -> [Encoded a]
encodeDirect [] = []
encodeDirect ks = map singles multiples
  where multiples = foldr f [] ks
        f x [] = [Multiple 1 x]
        f x l@((Multiple y v):xs)
          | x == v = (Multiple (y + 1) v) : xs
          | otherwise = Multiple 1 x : l
        f _ _ = undefined
        singles (Multiple 1 v) = Single v
        singles xs = xs

{-
Problem 14

(*) Duplicate the elements of a list.

Example:

* (dupli '(a b c c d))
(A A B B C C C C D D)

Example in Haskell:

> dupli [1, 2, 3]
[1,1,2,2,3,3]
-}
group14 :: TestTree
group14 = testGroup "duplicate elements of a list" [
    testCase "empty list" $
      [] @=? dupli ([] :: [Int])
    , testCase "full example" $
      [1, 1, 2, 2, 3, 3] @=? dupli [1, 2, 3 :: Int]
  ]

dupli :: [a] -> [a]
dupli xs = xs >>= (replicate 2)




group15 :: TestTree
group15 = testGroup "15" []
group16 :: TestTree
group16 = testGroup "16" []
group17 :: TestTree
group17 = testGroup "17" []
group18 :: TestTree
group18 = testGroup "18" []
group19 :: TestTree
group19 = testGroup "19" []
group20 :: TestTree
group20 = testGroup "20" []
group21 :: TestTree
group21 = testGroup "21" []
group22 :: TestTree
group22 = testGroup "22" []




