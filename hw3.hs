import Data.Word

-- You may import other libraries for this assignment; do so below this line
import Data.List 

{-

 Name: William Das      
 Uni: whd2108

 Collaborators: I wrote all this code myself, and consulted the below references, course slides, and ED posts. Also got suggestions from TAs/Professor from ED posts.

 References: 
 http://learnyouahaskell.com/making-our-own-types-and-typeclasses#type-parameters
 http://learnyouahaskell.com/functors-applicative-functors-and-monoids (functor reference)
 https://hackage.haskell.org/package/base-4.17.0.0/docs/Data-List.html#g:6 (Data.List reference)
 http://learnyouahaskell.com/starting-out
 https://wiki.haskell.org/Converting_numbers
 http://zvon.org/other/haskell/Outputprelude/^_o.html
 http://zvon.org/other/haskell/Outputprelude/quotRem_f.html (TA/Professor suggestion)

 I also looked at all class slides up to 10/23, particularly on recursion and casting/conversions with fromIntegral, as well as 
 ED Posts/Private posts (answers from Professor + TAs)


 ------------------------------

 COMS 4995 002 Parallel Function Programming

 Homework 3

 Due at 11:59 PM Sunday, October 23, 2022

 Modify this file with your solutions and submit it on Courseworks

 You may use functions, etc. from the Standard Prelude, but no
 other libraries

 Do not modify the type signatures for any of the provided functions.

 Above, include your name, UNI, list of people with whom your spoke about the
 assignment, and online references your consulted.

 Write your code alone.  You many consult the instructor, TAs, and other
 students, but do not copy/modify other's code.

 Please do not delete or modify any of the block comments below (i.e.,
 {- -} comments) as we use them to identify where your solutions begin and end.

 Feel free to add and delete single-line comments (i.e., --)

 Put any helper functions you add for a particular problem between
 the comment for the problem and the comment for the next problem

 -----

 Grading: 70% correctness: first and foremost, it needs to be correct
          30% style: is it readable, functional, concise?

 Since this homework is a bit short, its overall class weight will be
 lower than others.

 Use lts-19.23 as the "resolver" for the Haskell Tool Stack.
 E.g., :  

 Your code should load under GHCi 9.0.2 with no warnings under -Wall, e.g.
 :set -Wall
 :l hw3

 ==============================

 In the first nine problems of this assignment, you will implement an
 infinite precision integer arithemtic class called "Int100" that uses
 lists of Word8 (unsigned bytes) to represent integers in base 100 (to
 simplify decimal printing)

 Each list of Word8 digits consists of at least one digit and the last
 element -- the most significant digit -- must be non-zero.

 E.g.,

 IntP [0,2] represents 200
 IntN [5]   represents -5
 IntZ       represents 0
 IntP []    is illegal (must have at least one digit)
 IntN [0]   is illegal (last element most be non-zero)
 IntP [1,0] is illegal (last element must be non-zero)
 IntP [100] is illegal (digits must be in [0..99])

-}

data Int100 = IntP [Word8]
            | IntN [Word8]
            | IntZ
    deriving Eq

{- 1) Write the toInt100 function, which converts an
      Integer to an Int100

      E.g.,

      toInt100 0 = IntZ
      toInt100 (-1) = IntN [1]
      toInt100 99 = IntP [99]
      toInt100 100 = IntP [0,1]
      toInt100 (-199) = IntN [99,1]

      This is the only problem in which you are allowed to use
      functions that operate on the Integer type.
 -}
toInt100 :: Integer -> Int100
toInt100 x 
    | x == 0     = IntZ 
    | x > 0      = IntP (toBase100 x)
    | otherwise  = IntN (toBase100 (negate x))

-- Assumes x is nonnegative
toBase100 :: Integer -> [Word8]
toBase100 x
    | x > 0     = fromIntegral (x `mod` 100) : toBase100 (x `div` 100)
    | otherwise = []

{- 2) Write an instance of Show (just the show function)
      for the Int100 type.  You may assume the Int100 object
      is not illegal, i.e., every digit in the lists is in the range [0..99]
      and every list ends in a non-zero digit.

      The implementation provided below is intended for you to use
      while you are debugging other functions

      E.g., 

      show IntZ         = "0"
      show (IntP [3,4]) = "403"
      show (IntN [42])  = "-42

      You may not use any existing Integer code in your solution.
-}

instance Show Int100 where
  show IntZ     = "0"
  show (IntP l) = toBase10 l
  show (IntN l) = "-" ++ toBase10 l

toBase10 :: [Word8] -> String 
toBase10 [] = ""
toBase10 a@(xs:_) = foldl (\acc (x, y) -> acc ++ 
              (if x == 0 then "" else show x) ++ 
              (if y == 0 then "00" else if y < 10 then "0" else "")) "" (zip (reverse a) ys) ++ 
              (if xs == 0 then "" else show xs)
  where
    (_:ys) = reverse a

-- For testing show
toBase10Int :: [Word8] -> Integer
toBase10Int x = foldr (\(xs, ys) acc -> acc + (fromIntegral ys * ((100 :: Integer) ^ (xs :: Integer)))) 0 $ zip [0..] x

{- Old version:
instance Show Int100 where
  show IntZ = "0"
  show (IntP l) = show $ toBase10 l
  show (IntN l) = show $ negate $ toBase10 l

-- Assumes x is nonnegative base-100 number
toBase10 :: [Word8] -> Integer
toBase10 x = foldr (\(xs, ys) acc -> acc + (fromIntegral ys * ((100 :: Integer) ^ (xs :: Integer)))) 0 $ zip [0..] x
-}

{- 3) Write a function addDigits that adds two positive integers
      represented as lists in base-100 form, LSD first (e.g., as in an Int100).
      Assume every list element is in [0..99] and that the last element, if
      any, is non-zero.

      Your function should produce a list whose last element is non-zero
      or the empty list.

      E.g.,

      addDigits [] [] = []
      addDigits [] [1,2,3]  = [1,2,3]
      addDigits [99,1] [1]  = [0,2]
      addDigits [99,99] [1] = [0,0,1]
      addDigits [99,99,99] [99,99] = [98,99,0,1]

      You may not use any existing Integer code in your solution.
-}

-- Addition algorithm: recurse using quotient + remainder
-- Use quotRem (Chris TA suggestion)
addDigits :: [Word8] -> [Word8] -> [Word8]
addDigits x y = addDigitsCarry x y 0

addDigitsCarry :: [Word8] -> [Word8] -> Word8 -> [Word8]
addDigitsCarry [] [] 0          = []
addDigitsCarry x [] 0           = x
addDigitsCarry [] x 0           = x
addDigitsCarry [] [] x          = [x]
addDigitsCarry [] x y           = addDigitsCarry [y] x 0
addDigitsCarry x [] y           = addDigitsCarry [] x y
addDigitsCarry (x:xs) (y:ys) cx = r : addDigitsCarry xs ys q
  where
    (q, r) = quotRem (x + y + cx) 100

{-
addDigitsCarry (x:xs) (y:ys) cx = fromIntegral ((ix + iy + ic) `mod` 100) : addDigitsCarry xs ys (fromIntegral ((ix + iy + ic) `div` 100))
  where
    ix = fromIntegral x :: Integer
    iy = fromIntegral y :: Integer
    ic = fromIntegral cx :: Integer
-}


{- 4) Write a function subDigits that subtracts the second positive
      integer from the first.  Assume both numbers are in base-100 form,
      the last element, if any, is non-zero, and that the
      first number is greater or equal to the second (i.e., that the result
      is non-negative)

      Your function should produce a list in base-100 form, i.e.,
      each list element is in [0..99] and the last list element, if any,
      is non-zero.

      E.g.,

      subDigits [] [] = []
      subDigits [1,2,3,4] [1,2,3,4] = []
      subDigits [1,2,3,4] [0,2,3,4] = [1]
      subDigits [0,0,0,0,1] [1] = [99,99,99,99]
      subDigits [0,0,0,1] [0,1] = [0,99,99]
      subDigits [99,99,0,99] [1,10,1,1] = [98,89,99,97]

      You may not use any existing Integer code in your solution.
-}
-- Subtraction algorithm: recursve with borrow boolean
-- modify comparators for use with Word8 --> x - y - 1 < 0 should be x < y + 1, x - y < 0 should be x < y
subDigits :: [Word8] -> [Word8] -> [Word8]
subDigits [] [] = [] 
subDigits x y = dropWhileEnd (\xs -> xs `mod` 100 == 0) res 
  where 
    res = subDigitsBorrow x y False

subDigitsBorrow :: [Word8] -> [Word8] -> Bool -> [Word8]
subDigitsBorrow [] [] _                  = []
subDigitsBorrow [] _ _                   = []
subDigitsBorrow x [] borrow              = subDigitsBorrow x [0] borrow
subDigitsBorrow a@(x:xs) b@(y:ys) borrow 
    | a == b    = []
    | borrow    = if x < y + 1 then x + 100 - 1 - y : subDigitsBorrow xs ys True else x - y - 1 : subDigitsBorrow xs ys False
    | otherwise = if x < y then x + 100 - y : subDigitsBorrow xs ys True else x - y : subDigitsBorrow xs ys False

{-
subDigitsBorrow a@(x:xs) b@(y:ys) borrow 
    | a == b    = []
    | otherwise = if x < r then x + 100 - r : subDigitsBorrow xs ys True else x - r : subDigitsBorrow xs ys False
  where
    r = if borrow then y + 1 else y
-}

{-
subDigitsBorrow a@(x:xs) b@(y:ys) borrow
    | a == b    = []
    | otherwise = if diff < 0 then fromIntegral (diff + 100) : subDigitsBorrow xs ys True else fromIntegral diff : subDigitsBorrow xs ys False
  where
    ix = fromIntegral x :: Integer
    iy = fromIntegral y :: Integer
    diff = if borrow then ix - iy - 1 else ix - iy
-}

{-
Old versions of functions (disregard):

subDigitsBorrow :: [Word8] -> [Word8] -> Bool -> [Word8]
subDigitsBorrow [] [] _ = []
subDigitsBorrow [] _ _  = []
subDigitsBorrow x [] borrow = subDigitsBorrow x [0] borrow
subDigitsBorrow a@(x:xs) b@(y:ys) borrow
  | a == b    = []
  | borrow    = if ix - iy - 1 < 0 then fromIntegral (ix - iy - 1 + 100) : subDigitsBorrow xs ys True else fromIntegral(ix - iy - 1) : subDigitsBorrow xs ys False
  | otherwise = if ix - iy < 0 then fromIntegral (ix - iy + 100) : subDigitsBorrow xs ys True else fromIntegral (ix - iy) : subDigitsBorrow xs ys False
where
  ix = fromIntegral x :: Integer
  iy = fromIntegral y :: Integer

subDigitsBorrow x [] borrow
    | borrow    = if ix - 1 < 0 then fromIntegral (ix - 1 + 100) : subDigitsBorrow xs [] True else fromIntegral (ix - 1) : subDigitsBorrow xs [] False
    | otherwise = fromIntegral x : subDigitsBorrow xs [] False
  where
    ix = fromIntegral(x :: Word8) :: Integer

Older version: 

subDigitsBorrow :: [Word8] -> [Word8] -> Bool -> [Word8]
subDigitsBorrow [] [] _ = []
subDigitsBorrow [] _ _  = []
subDigitsBorrow (xs:ys) [] borrow
    | borrow    = fromIntegral ((fromIntegral (xs :: Word8) + 100 - 1) :: Integer) : subDigitsBorrow ys [] newBorrow
    | otherwise = fromIntegral (xs :: Word8) : subDigitsBorrow ys [] newBorrow
  where 
    intX = fromIntegral (xs :: Word8) :: Integer 
    newBorrow = intX <= 0 || intX >= 100 
subDigitsBorrow (x:xs) (y:ys) borrow = diff : subDigitsBorrow xs ys newBorrow
  where
    diffXY = (fromIntegral (x :: Word8) :: Integer) - (fromIntegral (y :: Word8) :: Integer)
    newBorrow = diffXY < 0
    diff
      | newBorrow = if borrow then fromIntegral (diffXY + 100 - 1) else fromIntegral (diffXY + 100)
      | otherwise = if borrow then fromIntegral (diffXY - 1) else fromIntegral diffXY
-}

{- 5) Write a function multDigit that multiplies a positive base-100 integer
      by an integer in the range [0..99].  Assume the last element of the
      base-100 list is non-zero and that the input integer is in the range
      [0..99]    

      E.g.,

      multDigit 0 [42] = []
      multDigit 1 [1,2,3] = [1,2,3]
      multDigit 2 [1,2,3] = [2,4,6]
      multDigit 99 [99,99,99] = [1,99,99,98]

      You may not use any existing Integer code in your solution.
-}

-- Multiplication algo: recurse with quotient and remainder
-- Use Word16 for multiplying and adding with quotRem for numbers larger than 255
-- map fromIntegral for converting from Word16 back to Word8
multDigit :: Word8 -> [Word8] -> [Word8]
multDigit _ [] = []
multDigit x y
    | x == 0    = []
    | x == 1    = y
    | otherwise = map fromIntegral (multDigitCarry (fromIntegral x) (map fromIntegral y) 0)

multDigitCarry :: Word16 -> [Word16] -> Word16 -> [Word16]
multDigitCarry _ [] 0      = []
multDigitCarry _ [] xs     = [xs]
multDigitCarry x (y:ys) cx = r : multDigitCarry x ys q
  where
    (q, r) = quotRem (x * y + cx) 100

{-
multDigitCarry x (y:ys) cx = fromIntegral ((ix * iy + ic) `mod` 100) : multDigitCarry x ys (fromIntegral ((ix * iy + ic) `div` 100))
  where
    ix = fromIntegral x :: Int
    iy = fromIntegral y :: Int
    ic = fromIntegral cx :: Int
-}



{- 6) Write a function multDigits that multiplies two positive base-100
      integers.  You may use multDigit and addDigits.

      E.g.,

      multDigits [] [1,2,3] = []
      multDigits [1,2,3] [] = []
      multDigits [1] [99,98,97] = [99,98,97]
      multDigits [0,1] [99,98,97] = [0,99,98,97]
      multDigits [0,2] [99,98,97] = [0,98,97,95,1]
      multDigits [99,99,99] [99,99,99] = [1,0,0,98,99,99]

      You may not use any existing Integer code in your solution.
-}

-- Multiply each factor of 100 in one number by digits in other and add results
-- Multiply by 100 is equivalent to prepending 0 to list (replicate 0)
-- Foldl + addDigits + multDigit -> multiply each digit by other number, prepend necessary amount of 0's based on factor of 100, and add result to accumulator
multDigits :: [Word8] -> [Word8] -> [Word8]
multDigits [] _ = []
multDigits _ [] = []
multDigits x y  = 
  foldl (\acc (cx, cy) -> addDigits (replicate cx 0 ++ multDigit (fromIntegral cy) y) acc) [] (zip [0..] x)


{- 7) Write a function compare100 that compares the magnitude of the
      two base-100 numbers given to it.
      Assume both input lists have non-zero last elements, but
      that they may be of different lengths.

      E.g.,

      compare100 [] [] = EQ
      compare100 [1] [] = GT
      compare100 [] [1] = LT
      compare100 [55,1] [55,1] = EQ
      compare100 [0,56] [1,55] = GT
      compare100 [0,0,0,2] [99,99,99,1] = GT
      compare100 [98,99,5] [99,98,5] = GT
      compare100 [99,98,5] [98,99,5] = LT
-}
-- Check lengths and if two lists are equal
-- If list length is equal, get rightmost non-equal element and compare values
compare100 :: [Word8] -> [Word8] -> Ordering
compare100 [] [] = EQ
compare100 x y 
    | length x > length y = GT
    | length x < length y = LT
    | x == y              = EQ
    | otherwise           = if cx > cy then GT else LT
        where
          ((cx, cy):_) = dropWhile (uncurry (==)) $ reverse $ zip x y 



{- 8) Write an instance of the Num type class for Int100.
      Include implementations of fromInteger, +, *, signum, abs,
      and negate. Use your toInt100, addDigits, subDigits, multDigits,
      and compare100 functions.

      E.g.,

     (fromInteger (-1234567)) :: Int100 = -1234567
     fromInteger 0 :: Int100 = 0
     fromInteger 42 :: Int100 = 42

     signum ((-5) :: Int100) = -1
     signum (0 :: Int100) = 0
     signum (421 :: Int100) = 1

     negate ((-123) :: Int100) = 123
     negate (0 :: Int100) = 0
     negate (453 :: Int100) = -453

     abs ((-123) :: Int100) = 123
     abs (123 :: Int100) = 123
     abs (0 :: Int100) = 0
 
     1234 + (4567 :: Int100) = 5801
     1234 + ((-1233) :: Int100) = 1
     1232 + ((-1233) :: Int100) = -1
     1234 - (1234 :: Int100) = 0
         
     99 * ((-99) :: Int100) = -9801
     (-998801) * (200 :: Int100) = -199760200
     (-16) * ((-32) :: Int100) = 512

     You may not use any existing Integer code in your solution.
-}
-- Addition + multiplication -> commutative
instance Num Int100 where
   fromInteger = toInt100
   IntP x + IntP y = IntP (addDigits x y)
   IntN x + IntN y = IntN (addDigits x y)
   IntP x + IntN y
       | compare100 x y == EQ = IntZ
       | compare100 x y == GT = IntP (subDigits x y)
       | otherwise            = IntN (subDigits y x)
   IntZ + x        = x
   x + y           = y + x

   IntZ * _        = IntZ
   IntP x * IntN y = IntN (multDigits x y)
   IntP x * IntP y = IntP (multDigits x y)
   IntN x * IntN y = IntP (multDigits x y)
   x * y           = y * x   
   
   signum (IntP _) = 1
   signum (IntN _) = -1
   signum IntZ     = 0

   negate (IntP x) = IntN x
   negate (IntN x) = IntP x
   negate IntZ = IntZ

   abs (IntP x)    = IntP x
   abs (IntN x)    = IntP x 
   abs IntZ        = IntZ

{- 9) Write an instance of the Ord type class for Int100.
      in particular, write an implementation of the compare
      function; Haskell will infer the rest.  Use your
      compare100 function.

      E.g.,

     (0 :: Int100) `compare` 0 = EQ
     (0 :: Int100) `compare` 1 = LT
     (1 :: Int100) `compare` 0 = GT
     (1 :: Int100) `compare` 1 = EQ
     ((-1) :: Int100) `compare` 1 = LT
     (1 :: Int100) `compare` (-1) = GT

     You may not use any existing Integer code in your solution.
-}
instance Ord Int100 where
  (IntP x) `compare` (IntP y) = compare100 x y
  (IntP _) `compare` (IntN _) = GT
  (IntP _) `compare` IntZ     = GT
  (IntN _) `compare` (IntP _) = LT
  (IntN x) `compare` (IntN y) = compare100 y x
  (IntN _) `compare` IntZ     = LT
  IntZ `compare` IntZ         = EQ
  IntZ `compare` (IntN _)     = GT
  IntZ `compare` (IntP _)     = LT

{- 10) Implement the Functor type classes for the BHeap and BTree types.

     Your solution should satisfy the functor properties:
     fmap id = id
     fmap ( f . g ) = fmap f . fmap g
-}
data BHeap a = BHeap [BTree a] deriving (Eq, Show, Read)
data BTree a = BNode Int a (BHeap a) deriving (Eq, Show, Read)

instance Functor BHeap where
  --fmap :: (a -> b) -> BHeap a -> BHeap b
  fmap f (BHeap x) = BHeap (map (fmap f) x)
  

instance Functor BTree where
  --fmap :: (a -> b) -> BTree a -> BTree b
  fmap f (BNode x y xs) = BNode x (f y) (fmap f xs)
