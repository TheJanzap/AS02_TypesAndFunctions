# Assignment 2 - Types and Functions

In the first part of this assignment you practice to find the types for given expressions.
In the second part you improve your programming skills with regard to recursion and higher order functions.

## Part 1: Types

Find the most general type of a given expression. In case you have trouble finding the correct types, read the provided CheckingTypes.pdf.

In case you do not know the type of a function which is used in the exercise, use ghci:
```
ghci> :t map
map :: (a -> b) -> [a] -> [b]
```
Try hard and finally check your solution with ghci. Example:
```
ghci> :t True`
True :: Bool
```

Assume `Int` as the type of whole numbers.
The expressions are becoming increasingly difficult:
```
True :: Bool
'X' :: ???
[True, False] :: ???
fst (True, 1) :: ???
fst :: ???
map even [1,2,3] :: ???
map snd [('a',True), ('b', False)] :: ???
map snd :: ???
map head :: ???
filter snd :: ???
head . snd :: ???
fst . (\(e,f,g) -> g) :: ???
map . map :: ???
map map :: ???
```



## Part 2: Functions
a) Open [Main.hs](./Main.hs) and implement the specified functions.

To test your implementations run the following command:
```
cabal test --test-show-details=direct
```

To experiment with code in GHCi you can run the following command:
```
cabal repl test:tests
```

b) Skim over the `Prelude`` documentation and inspect what list functions are available:
https://hackage-content.haskell.org/package/base-4.20.1.0/docs/Prelude.html#g:13

Hint:
You will find functions with type signatures like:
`null :: Foldable t => t a -> Bool`
This means that `null` is defined for every type `t` which implements the `Foldable` typeclass.
Our List type has a `Foldable` implementation, thus you can read the above as:
`null :: List a -> Bool` or using Haskell list syntax `null :: [a] -> Bool`.

We will cover type classes in detail in upcoming lectures.


c) Illustrations for some higher order list functions.
https://colah.github.io/posts/2015-02-DataList-Illustrated/

d) More exercises can be found at https://en.wikibooks.org/wiki/Haskell/Higher-order_functions

