{-

Problem 2: Word Ladder

Write a program that takes the name of a dictionary file and starting
and ending words and constructs the shortest "word ladder," i.e., a
sequence of dictionary words that differ by exactly one changed
character that transforms the starting word into the ending word.
Unlike some word ladder puzzles, your program should not consider
adding or removing letters.

Your program should read a Unix-style /usr/dict/words file (i.e., a
sorted, newline-separated list of valid words), and only consider
lowercase-only words (i.e., no capitalized proper nouns).  You may
also ignore words with punctuation, e.g., abase is fine, but ignore
aardvark's, ASCII, and Abby.

The user must supply a valid dictionary file and start and end words
that are the same length, all lowercase, and both in the dictionary.
Your program should print an error message and terminate with a
non-zero error code if supplied with erroneous arguments.

Search for ladders of at most 20 words, returning an error message if
one cannot be found with the given dictionary.  For a given pair of
words and dictionary, there is often more than one minimum-length
ladder; you may return any of them.

Here is an example run; your program should work with other
dictionaries, too.

$ wget https://raw.githubusercontent.com/eneko/data-repository/master/data/words.txt
$ stack --resolver lts-19.23 ghc -- --make -Wall -O wordLadder.hs
$ ./wordLadder words.txt fool sage
fool
foot
fort
fore
fare
fage
sage
$ ./wordLadder words.txt computer solution
Unable to find a ladder in 20
$ ./wordLadder
Usage: wordLadder <dictionary-filename> <from-word> <to-word>
$ ./wordLadder foo
Usage: wordLadder <dictionary-filename> <from-word> <to-word>
$ ./wordLadder foo bar
Usage: wordLadder <dictionary-filename> <from-word> <to-word>
$ ./wordLadder foo bar baz
wordLadder: foo: openFile: does not exist (No such file or directory)
$ ./wordLadder words.txt fooa barb
"fooa" not in dictionary
$ ./wordLadder words.txt barb fooa
"fooa" not in dictionary
$ ./wordLadder words.txt bar none
"bar" and "none" must be the same length

As for the wordFreq problem, make your solution correct, readable, and
fast (in order of importance).  Again, we will classify the
performance of your solution into one of three categories:
substantially faster than our reference solution, about the same, and
substantially slower.

Our 68-line reference solution uses a BFS-style search implemented
with functions from System.IO, System.Exit, Data.Char,
System.Environment, Data.Set, Control.Monad, and Data.List.

For reference, the fool->sage example above took about 220 ms on my
desktop machine.  Our solution uses one fairly straightforward trick
to improve its performance, but is otherwise untuned.

-}
{-# LANGUAGE TupleSections #-}
import System.Environment
import qualified Data.ByteString.Char8 as C
import Data.Char
import Control.Monad()
import qualified Data.Set as Set
import Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Maybe

-- Only process words equal to length of two strings
-- Use map with connecting representations of words to optimize search/access of neighboring words (differ by one letter)
-- Simple BFS: store ladder paths in queue, enqueue ladders with neighbors until last letter in deqeued ladder is target word -> return that ladder (~ 220ms on my machine for fool/sage)
-- Modified BFS (slightly faster, less space) -> grab pairs of tuples instead of complete ladders up to target word, then backtrack/DFS one path from target word to original word 
-- (~ 170ms on my machine for fool/sage)
main :: IO ()
main = do
  args <- getArgs
  if length args == 3 
      then do 
          let (fname:fromWord:toWord:_) = args
          input <- C.readFile fname

          let wordLength = length fromWord
          let wordsDict = filter (\x -> not (C.any isPunctuation x) 
                                            && not (C.any isUpper x) 
                                            && C.length x == wordLength) 
                          $ C.words input

          let out | length fromWord /= length toWord    = mapM_ putStr [show fromWord, " and ", show toWord, " must be the same length\n"]
                  | C.pack fromWord `notElem` wordsDict = mapM_ putStr [show fromWord, " not in dictionary\n"]
                  | C.pack toWord `notElem` wordsDict   = mapM_ putStr [show toWord, " not in dictionary\n"]
                  | otherwise                           = printLadder (C.pack fromWord) (C.pack toWord) wordsDict

          out
      else
          putStrLn "Usage: wordLadder <dictionary-filename> <from-word> <to-word>" 

printLadder :: C.ByteString -> C.ByteString -> [C.ByteString] -> IO ()
printLadder fromWord toWord wordsDict =
  case res of
      Just x -> out
        where
          path                    = backtrackPath fromWord toWord x
          out | length path <= 20 = mapM_ C.putStrLn path
              | otherwise         = putStrLn "Unable to find a ladder in 20"
                      
      Nothing -> putStrLn "Unable to find a ladder in 20" 
  where
    wordMap = buildMap wordsDict
    res = getTupleLevels fromWord toWord wordMap

-- DFS style backtrack for one singular path with min-depth, starting from target word
-- Params: targetWord currWord tupleLevels
backtrackPath :: C.ByteString -> C.ByteString -> [[(C.ByteString, C.ByteString)]] -> [C.ByteString]
backtrackPath _ _ []       = []
backtrackPath targetWord currWord (x:xs)
  | currWord == targetWord = [currWord]
  | otherwise              = case find (\(_, b) -> b == currWord) x of
                                 Just (a, b) -> backtrackPath targetWord a xs ++ [b]
                                 Nothing -> []

-- Get all levels up from start to target word
getTupleLevels :: C.ByteString -> C.ByteString -> Map.Map C.ByteString [C.ByteString] -> Maybe [[(C.ByteString, C.ByteString)]]
getTupleLevels fromWord toWord wordMap = bfsPairs toWord wordMap [[(C.pack "_", fromWord)]] (Set.fromList [fromWord])

-- BFS -> get tuples for each new level in graph, each with (word in previous level, neighbor) for each neighbor of a word in previous level
bfsPairs :: C.ByteString -> Map.Map C.ByteString [C.ByteString] -> [[(C.ByteString, C.ByteString)]] -> Set.Set C.ByteString -> Maybe [[(C.ByteString, C.ByteString)]]
bfsPairs _ _ [] _           = Nothing
bfsPairs toWord wordsDict allTuples@(x:_) seen
  | toWord `elem` map snd x = Just allTuples
  | null x                  = Nothing
  | otherwise               = bfsPairs toWord wordsDict newTuples newSeen
  where
    (nextLevel, newSeen) = getNextLevel x wordsDict seen
    newTuples            = nextLevel : allTuples

-- BFS -> get neighboring words as tuples in next level
getNextLevel :: [(C.ByteString, C.ByteString)] -> Map.Map C.ByteString [C.ByteString] -> Set.Set C.ByteString -> ([(C.ByteString, C.ByteString)], Set.Set C.ByteString)
getNextLevel currPath wordsDict excludeWords = (newLevel, newSeen)
  where
    newNeighbors = map (\(_, y) -> filter (`Set.notMember` excludeWords) $ getNeighbors y wordsDict) currPath
    newLevel     = concat $ zipWith (\(_, x) y -> map (x, ) y) currPath newNeighbors
    newSeen      = Set.union excludeWords $ Set.fromList (map snd newLevel)

-- Build word map/graph using different representations of words
buildMap :: [C.ByteString] -> Map.Map C.ByteString [C.ByteString]
buildMap x = Map.fromListWith (++) $ concatMap (\xs -> map (, [xs]) $ wordRepresentations xs) x

-- Get all representations for word ("what" -> ["@hat", "w@at", "wh@t", "wha@"])
wordRepresentations :: C.ByteString -> [C.ByteString]
wordRepresentations x = map (\i -> C.concat [C.take i x, C.pack "@", C.drop (i + 1) x]) [0..(C.length x - 1)]

-- Get all neighbors of word
getNeighbors :: C.ByteString -> Map.Map C.ByteString [C.ByteString] -> [C.ByteString]
getNeighbors x wordMap = concatMap (\w -> Data.Maybe.fromMaybe [] $ Map.lookup w wordMap) (wordRepresentations x)
