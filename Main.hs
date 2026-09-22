{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# HLINT ignore "Evaluate" #-}
{-# HLINT ignore "Use list literal" #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# HLINT ignore "Eta reduce" #-}

module Main (main) where

import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Test.QuickCheck (Testable (property))
import Prelude hiding (Foldable)

todo :: a
todo = error "TODO"

-------------------------------------------------------------------------------
-- 0. Types
-------------------------------------------------------------------------------

-- Implement the two functions with the given types.
-- There is only one solution each which is a total and terminating.
-- Follow the types!
-- f: (a, b) -> c
-- g: c -> d
-- tuple: (a, b)
-- returns: d
-- `f` takes a tuple so input that. It then returns a `c`.
-- `g` takes a `c` and outputs a `d`!
riddleA :: ((a, b) -> c) -> (c -> d) -> (a, b) -> d
riddleA f g tuple = g (f tuple)

-- val: a
-- f: (a -> b) -> c
-- g: a -> a -> b which is a -> (a -> b)
-- returns: c
-- `g` takes an `a` and returns a function `(a -> b)`.
-- f takes that function and returns a `c`!
riddleB :: a -> ((a -> b) -> c) -> (a -> a -> b) -> c
riddleB val f g = f (g val)

-------------------------------------------------------------------------------
-- 1. Recursion on Lists
-------------------------------------------------------------------------------

-- Implement the function `myLength`. It returns the length of a list:
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs 

myLengthSpec :: Spec
myLengthSpec =
  describe "myLength" $ do
    it "myLength [] == 0" $ myLength [] `shouldBe` 0
    it "myLength [1,2] == 2" $ myLength ([1, 2] :: [Int]) `shouldBe` 2
    it "behaves like length" $ property $ \(l :: [Int]) -> myLength l == myLength l

-- Implement the function myReverse. It reverses a list:
-- Linear runtime here instead of quadratic with `++`
myReverse :: [a] -> [a]
myReverse = prepend []
  where
    prepend acc [] = acc
    prepend acc (x:xs) = prepend (x:acc) xs

myReverseSpec :: Spec
myReverseSpec =
  describe "myReverse" $ do
    it "myReverse [] == []" $ myReverse [] `shouldBe` ([] :: [Int])
    it "myReverse [1,2] == [2,1]" $ myReverse [1, 2] `shouldBe` ([2, 1] :: [Int])
    it "behaves like reverse" $ property $ \(l :: [Int]) -> myReverse l == reverse l

-- Implement the function drop. It drops the first n elements.
-- It returns the list unchanged for negative n.
myDrop :: Int -> [a] -> [a]
myDrop n xs
  | n <= 0      = xs
myDrop _ []     = []
myDrop n (_:xs) = myDrop (n - 1) xs

myDropSpec :: Spec
myDropSpec =
  describe "myDrop" $ do
    it "myDrop 0 [] == []" $ myDrop 0 [] `shouldBe` ([] :: [Int])
    it "myDrop 2 [] == []" $ myDrop 2 [] `shouldBe` ([] :: [Int])
    it "myDrop 2 [1,2] == []" $ myDrop 2 [1, 2] `shouldBe` ([] :: [Int])
    it "myDrop 2 [1,2,3] == [3]" $ myDrop 2 [1, 2, 3] `shouldBe` ([3] :: [Int])
    it "myDrop 7 [1,2,3] == []" $ myDrop 7 [1, 2, 3] `shouldBe` ([] :: [Int])
    it "myDrop -3 [1,2] == [1,2]" $ myDrop (-3) [1, 2] `shouldBe` ([1, 2] :: [Int])
    it "behaves like drop" $ property $ \(n :: Int, l :: [Int]) -> myDrop n l == drop n l

-------------------------------------------------------------------------------
-- 2. Recursion on Trees (https://en.wikipedia.org/wiki/Tree_traversal)
-------------------------------------------------------------------------------

data Bin a = Leaf a | Fork (Bin a) a (Bin a) deriving (Eq, Show)

-- Example tree with Chars as elements.
-- It may help to draw it on paper.
--     d
--    / \
--   f   b
--  / \ / \
-- g  e a  c
exTree :: Bin Char
exTree =
  Fork
    ( Fork
        (Leaf 'a')
        'b'
        (Leaf 'c')
    )
    'd'
    ( Fork
        (Leaf 'e')
        'f'
        (Leaf 'g')
    )

-- Implement the function preorder. It traverses a binary tree pre-order:
preorder :: Bin a -> [a]
preorder (Leaf value) = [value]
preorder (Fork left value right) = [value] ++ preorder left ++ preorder right

preorderSpec :: Spec
preorderSpec =
  describe "preorder" $ do
    it "preorder (Leaf 1) == [1]" $ preorder (Leaf 1) `shouldBe` ([1] :: [Int])
    it "preorder (Fork (Leaf 'a') 'b' (Leaf 'c')) == \"bac\"" $ preorder (Fork (Leaf 'a') 'b' (Leaf 'c')) `shouldBe` "bac"
    it "preorder exTree == \"dbacfeg\"" $ preorder exTree `shouldBe` "dbacfeg"

-- Implement the function inorder. It traverses a binary tree in-order:
inorder :: Bin a -> [a]
inorder (Leaf value) = [value]
inorder (Fork left value right) = inorder left ++ [value] ++ inorder right

inorderSpec :: Spec
inorderSpec =
  describe "inorder" $ do
    it "inorder (Leaf 1) == [1]" $ inorder (Leaf 1) `shouldBe` ([1] :: [Int])
    it "inorder (Fork (Leaf 'a') 'b' (Leaf 'c')) == \"abc\"" $ inorder (Fork (Leaf 'a') 'b' (Leaf 'c')) `shouldBe` "abc"
    it "inorder exTree == \"abcdefg\"" $ inorder exTree `shouldBe` "abcdefg"

-- Implement the function postorder. It traverses a binary tree post-order:
postorder :: Bin a -> [a]
postorder (Leaf value) = [value]
postorder (Fork left value right) = postorder left ++ postorder right ++ [value]

postorderSpec :: Spec
postorderSpec =
  describe "postorder" $ do
    it "postorder (Leaf 1) == [1]" $ postorder (Leaf 1) `shouldBe` ([1] :: [Int])
    it "postorder (Fork (Leaf 'a') 'b' (Leaf 'c')) == \"abc\"" $ postorder (Fork (Leaf 'a') 'b' (Leaf 'c')) `shouldBe` "acb"
    it "postorder exTree == \"acbegfd\"" $ postorder exTree `shouldBe` "acbegfd"

-- Now we build a small calculator for rationals:
data Rat = Int :/: Int deriving (Show, Eq)

-- Supported operations:
data Op = ADD | SUB | MUL | DIV deriving (Show, Eq)

-- Type for expressions:
data Expr
  = Val Rat
  | Bin Op Expr Expr
  deriving (Show, Eq)

-- Helper function to simplify/shorten a rational:
shorten :: Rat -> Rat
shorten (n :/: d) = (n `div` f) :/: (d `div` f)
  where
    f = gcd n d

-- The following function evaluates an operation on two rationals:
-- Complete the cases for SUB, MUL and DIV
-- Hint: DIV can easy be implemented by MUL with the reciprocal
evalOp :: Op -> Rat -> Rat -> Rat
evalOp ADD (ln :/: ld) (rn :/: rd) = shorten (((ln * rd) + (rn * ld)) :/: (ld * rd))
evalOp SUB (ln :/: ld) (rn :/: rd) = shorten (((ln * rd) - (rn * ld)) :/: (ld * rd))
evalOp MUL (ln :/: ld) (rn :/: rd) = shorten ((ln * rn) :/: (ld * rd))
evalOp DIV l (rn :/: rd) = evalOp MUL l (rd :/: rn)

evalOpSpec :: Spec
evalOpSpec =
  describe "evalOp" $ do
    it "evalOp ADD (1:/:2) (1:/:4) == (3:/:4)" $ evalOp ADD (1 :/: 2) (1 :/: 4) `shouldBe` (3 :/: 4)
    it "evalOp SUB (1:/:2) (1:/:4) == (1:/:4)" $ evalOp SUB (1 :/: 2) (1 :/: 4) `shouldBe` (1 :/: 4)
    it "evalOp MUL (1:/:2) (1:/:4) == (1:/:8)" $ evalOp MUL (1 :/: 2) (1 :/: 4) `shouldBe` (1 :/: 8)
    it "evalOp DIV (1:/:2) (1:/:4) == (1:/:8)" $ evalOp DIV (1 :/: 2) (1 :/: 4) `shouldBe` (2 :/: 1)

-- Now implement the `eval` function, which evaluates an expression:
eval :: Expr -> Rat
eval (Val r) = r
eval (Bin op l r) = evalOp op (eval l) (eval r)

-- Example expression:
-- ((1/2) + (1/4)) * ((1/6) / (2/1))
e :: Expr
e =
  Bin
    MUL
    ( Bin
        ADD
        (Val (1 :/: 2))
        (Val (1 :/: 4))
    )
    ( Bin
        DIV
        (Val (1 :/: 6))
        (Val (2 :/: 1))
    )

evalSpec :: Spec
evalSpec =
  describe "eval" $ do
    it "eval (Val (1:/:2)) == (1:/:2)" $ eval (Val (1 :/: 2)) `shouldBe` (1 :/: 2)
    it "eval e == (1:/:16)" $ eval e `shouldBe` (1 :/: 16)

-------------------------------------------------------------------------------
-- 3. Higher order functions on lists
-------------------------------------------------------------------------------

-- Data type for examples
data Paradigm = Functional | Imperative deriving (Eq, Show)

data Language = Language {name :: String, paradigm :: Paradigm} deriving (Eq, Show)

languages :: [Language]
languages =
  [ Language "Haskell" Functional,
    Language "Java" Imperative,
    Language "Lisp" Functional,
    Language "Python" Imperative
  ]

-- Implement your own version of the function `map`.
-- It applies a function to every element in a list.
myMap :: (a -> b) -> [a] -> [b]
myMap _ []     = []
myMap f (x:xs) = f x : myMap f xs 

myMapSpec :: Spec
myMapSpec =
  describe "myMap" $ do
    it "myMap (\\x -> x + 1) [] == []" $ myMap (\x -> x + 1) [] `shouldBe` ([] :: [Int])
    it "myMap (\\x -> x + 1) [1,2,3] == [2,3,4]" $ myMap (\x -> x + 1) [1, 2, 3] `shouldBe` ([2, 3, 4] :: [Int])

-- Implement your own version of the function `filter`.
-- It keeps only the elements which satisfy the predicate.
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter f (x:xs) | f x       = x : myFilter f xs
                  | otherwise = myFilter f xs 

myFilterSpec :: Spec
myFilterSpec =
  describe "myFilter" $ do
    it "myFilter even [] == []" $ myFilter even [] `shouldBe` ([] :: [Int])
    it "myFilter even [1,2,3,4] == [2,4]" $ myFilter even [1, 2, 3, 4] `shouldBe` ([2, 4] :: [Int])

-- Implement the function `squares`. It squares every element in a list.
-- Make use of the predefined function `map`:
squares :: [Int] -> [Int]
squares xs = map (\x -> x * x) xs 

squaresSpec :: Spec
squaresSpec =
  describe "squares" $ do
    it "squares [] == []" $ squares [] `shouldBe` ([] :: [Int])
    it "squares [1,2,3] == [1,4,9]" $ squares [1, 2, 3] `shouldBe` ([1, 4, 9] :: [Int])

-- Implement the function `names`. It extracts the names of the languages.
-- Make use of the predefined function `map`:
names :: [Language] -> [String]
names ls = map name ls

namesSpec :: Spec
namesSpec =
  describe "names" $ do
    it "names [] == []" $ names [] `shouldBe` ([] :: [String])
    it "names languages == [\"Haskell\", \"Java\", \"Lisp\", \"Python\"]" $ names languages `shouldBe` ["Haskell", "Java", "Lisp", "Python"]

-- Implement the function `evens`. It keeps only the even values of a list.
-- Use the function `filter` and the `even` function:
evens :: [Int] -> [Int]
evens xs = filter even xs

evensSpec :: Spec
evensSpec =
  describe "evens" $ do
    it "evens [] == []" $ evens [] `shouldBe` ([] :: [Int])
    it "evens [1,2,3,4] == [2,4]" $ evens [1, 2, 3, 4] `shouldBe` ([2, 4] :: [Int])

-- Implement the function `likes`. It keeps only the functional languages:
-- Use the function `filter` and write the predicate as a lambda expression:
likes :: [Language] -> [Language]
likes ls = filter (\l -> (paradigm l) == Functional) ls

likesSpec :: Spec
likesSpec =
  describe "likes" $ do
    it "likes [] == []" $ likes [] `shouldBe` ([] :: [Language])
    it "likes languages == [Language \"Haskell\" Functional, Language \"Lisp\" Functional]" $
      likes languages `shouldBe` [Language "Haskell" Functional, Language "Lisp" Functional]

-- Implement the function `foldrLength`. It computes the lengths of a list.
-- Use the function `foldr`:
-- (a -> b -> b): a = list element, b: accumulator, previous value. The function modifies the accumulator and returns it.
-- -> b: Initial value of the accumulator
-- -> t a: List element (must be a `Foldable` structure)
-- -> b: Returns the final value of the accumulator
foldrLength :: [a] -> Int
foldrLength xs = foldr (\_ acc -> acc + 1) 0 xs

foldrLengthSpec :: Spec
foldrLengthSpec =
  describe "foldrLength" $ do
    it "behaves like length" $ property $ \(l :: [Int]) -> foldrLength l == length l

-- Implement the function `foldrMap`. It has the same behavior like `map`.
-- Use the function `foldr`:
-- a: current list element, xs': already processed list elements
foldrMap :: (a -> b) -> [a] -> [b]
foldrMap f xs = foldr (\a xs' -> f a : xs') [] xs

foldrMapSpec :: Spec
foldrMapSpec =
  describe "foldrMap" $ do
    it "foldrMap (+1) [] == []" $ foldrMap (+ 1) [] `shouldBe` ([] :: [Int])
    it "foldrMap (+1) [1,2,3] == [2,3,4]" $ foldrMap (+ 1) [1, 2, 3] `shouldBe` ([2, 3, 4] :: [Int])
    it "foldrMap even [1,2,3] == [2]" $ foldrMap even ([1, 2, 3] :: [Int]) `shouldBe` [False, True, False]

-------------------------------------------------------------------------------
-- 4. Combinators
-------------------------------------------------------------------------------

-- Implement the "pipe operator" `|>` which allows to combine functions from left to right:
-- The following should compile when uncommented and evaluate to 5.

res :: Int
res = (fst |> head |> length) (["hallo", "bla"], True)

-- Hints:
-- 1. First write down the type signature.
-- 2. The operator needs to be surrounded by parenthesis in the type signature:
-- Example: (<+>) :: Int -> Int -> Int

-- The pipe operator builds a "translator". You have two functions. You input `a` into f1 and want `c` from f2.
-- The pipe links the output of f1 to the input of f2. 
-- Using a lambda here to make it more clear that we return a function, not a value
(|>) :: (a -> b) -> (b -> c) -> (a -> c)
(|>) f1 f2 = \a -> f2 (f1 a)
-- Or with syntactic sugar with the dot operator. 
-- It is basically already a pipe operator, but its arguments are processed in reverse order:
-- (|>) first second = sevond . first

-- Implement the function `flip'`.
-- It takes a function and flips its first two arguments.
flip' :: (a -> b -> c) -> (b -> a -> c)
flip' f = \b a -> f a b

flip'Spec :: Spec
flip'Spec =
  describe "flip'" $ do
    it "flip' take \"abcd\" 2 == \"ab\"" $ flip' take "abcd" 2 `shouldBe` "ab"

-- Implement the function `curry'`.
-- It converts a function which takes a pair to a function which takes the arguments one after another.
-- f is the function ((a, b) -> c). Then we provide the two arguments a and b as input
curry' :: ((a, b) -> c) -> (a -> b -> c)
curry' f = \a b -> f (a, b)

curry'Spec :: Spec
curry'Spec =
  describe "curry'" $ do
    it "curry' fst 'a' True == 'a'" $ curry' fst 'a' True `shouldBe` 'a'

-- Implement the function `uncurry' :: (a -> b -> c) -> ((a,b) -> c)`.
-- It is the inverse of `curry'`: `curry' . uncurry' == id`:
-- f is the function (a -> b -> c). Then we provide the tuple (a, b) as input.
uncurry' :: (a -> b -> c) -> ((a, b) -> c)
uncurry' f  = \(a, b) -> f a b

uncurry'Spec :: Spec
uncurry'Spec =
  describe "uncurry'" $ do
    it "uncurry' (&&) (True,False) == False" $ uncurry' (&&) (True, False) `shouldBe` False

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "1. Recursion on lists" $ do
    myLengthSpec
    myReverseSpec
    myDropSpec
  describe "2. Recursion on trees" $ do
    preorderSpec
    inorderSpec
    postorderSpec
    evalOpSpec
    evalSpec
  describe "3. Higher order functions on lists" $ do
    myMapSpec
    myFilterSpec
    squaresSpec
    namesSpec
    evensSpec
    likesSpec
    foldrLengthSpec
    foldrMapSpec
  describe "4. Combinators" $ do
    flip'Spec
    curry'Spec
    uncurry'Spec
