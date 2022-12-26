{-

Problem 1: Word Frequency Counter

Write a program that prints the 40 most common words and the number of
times they appear in the given text file.  Compile this into a
standalone executable that takes a single filename as a command-line
argument.  Print a usage message if the user does not supply exactly
one argument.

To find words, discard all non-alphabetic characters aside from
whitespace and treat what's left as lowercase.  E.g., treat "Stephen's
humor-challenged" as two words: "stephens" and "humorchallenged"

You may use any of the System or Data modules.  Ask permission from
the instructor on Ed before you use any other modules.  You may not
use the Data.Text.WordCount module.

In my 32-line reference solution, I used Data.Map.fromListWith,
Data.List.sortBy, readFile, System.Exit.die,
System.Environment.getArgs, and System.Environment.getProgName.

In addition to making your solution correct and readable, try to make
your solution go fast, but leave it single-threaded.  We will classify
solutions into three performance categories and assign 10% of the
score based on these categories:

1) Roughly the same as my reference solution (about 2s on the
   Shakespeare example);
2) Noticably faster than the reference solution
3) Noticably slower than the reference solution

E.g., on the Complete Works of Shakespeare

$ stack --resolver lts-19.23 ghc -- --make -Wall -O wordFreq
$ ./wordFreq
Usage: wordFreq <filename>
$ wget http://www.gutenberg.org/files/100/100-0.txt
$ ./wordFreq 100-0.txt

This particular file is UTF-8 encoded, but your solution only needs to work
with ASCII (8-bit) characters.  To reincode it in ASCII,

$ iconv -f UTF-8 -t ASCII//TRANSLIT -o 100-0.ascii 100-0.txt 
$ ./wordFreq 100-0.ascii

30247 the
28395 and
21971 i
20936 to
18815 of
16225 a
14441 you
13181 my
12261 in
11777 that
9717 is
9064 not
8527 with
8264 me
8193 it
8191 for
7584 his
7365 be
7169 this
7066 your
6828 he
6751 but
6268 have
6178 as
5845 thou
5551 him
5452 so
5290 will
4757 what
4597 her
4337 thy
4214 all
4094 by
4072 no
3912 do
3850 shall
3805 if
3722 are
3557 we
3394 thee

My solution disagreed about the number of occurences of "a" in the
UTF-8 file (16215) and the ASCII file (16225), but agreed about all
the other counts.

For reference, my straightforward, untuned solution took about 2s to
run on the Shakespeare file.

-}
{-# LANGUAGE TupleSections #-}
 
import System.Environment
import Control.Monad () 
import qualified Data.ByteString.Char8 as C
import qualified Data.Map.Strict as Map
import Data.Char
import Data.List

-- iconv -c -f utf-8 -t ascii < 100-0.txt > 100-0.ascii
-- Use Data.Map.Strict: fromListWith
-- Use C.ByteString
main :: IO ()
main = do  
    args <- getArgs
    if length args == 1
        then do
            let (fname:_) =  args
            input <- C.readFile fname
            let freq = wordFreqMap input
            mapM_ (\(k, v) -> putStrLn $ show v ++ " " ++ C.unpack k) $ take 40 freq
    else do
        putStrLn "Usage: wordFreq <filename>"

wordFreqMap :: C.ByteString -> [(C.ByteString, Int)]
wordFreqMap input = sortBy (\(_, y) (_, ys) -> compare ys y) 
                    $ Map.toList 
                    $ Map.fromListWith (+) 
                    $ map (, 1)  
                    $ C.words  
                    $ C.map toLower 
                    $ C.filter (\x -> isAlpha x || isSpace x) 
                    input

