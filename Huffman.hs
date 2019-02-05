-- DO NOT MODIFY THE FOLLOWING LINES

module Huffman(HuffmanTree, characterCounts, huffmanTree, codeTable, compress, decompress) where

import Table
import PriorityQueue

import Test.HUnit

{- a bit code (of a character or string) is represented by a list of Booleans
   INVARIANT:
     the bit code is a concatenation of (0 or more) valid code words for some Huffman tree
 -}
type BitCode = [Bool]

-- END OF DO NOT MODIFY ZONE

--------------------------------------------------------------------------------

{- insert1 elem list
   Sorts the given element into the given list.
   RETURNS: A list with the given element sorted into it. 
   EXAMPLES: insert1 5 [1,2,3,4,6] = [1,2,3,4,5,6]
-}
insert1 :: Ord a => a -> [a] -> [a]
insert1 elem [] = [elem]
insert1 elem (x:xs) 
  | elem < x = elem:x:xs
  | otherwise = x:(insert1 elem xs)

--insertionSort s
--Sorts the list s
--Trivial function
insertionSort :: Ord a => [a] -> [a]
insertionSort xs = insertionSortAux [] xs where
   {- insertionSortAux sortedList newList
   Takes two lists and merge them into one sorted list
   RETURNS: A sorted list, resulting from the two given lists. 
   EXAMPLES: insertionSortAux [2,5,6,3] [1,4,8] = [1,2,3,4,5,6,8]
   -}
  insertionSortAux :: Ord a => [a] -> [a] -> [a]
  insertionSortAux sortedList [] = sortedList
  insertionSortAux sortedList (x:xs) = insertionSortAux (insert1 x sortedList) xs

{- characterCounts s
   RETURNS: a table that maps each character that occurs in s to the number of
            times the character occurs in s
   EXAMPLES: characterCounts "hello there" = T [('t',1),('r',1),('o',1),('l',2),('h',2),('e',3),(' ',1)]
 -}
characterCounts :: String -> Table Char Int
characterCounts s = characterCountsAux (insertionSort s) 1 where
  {- characterCountsAux s q
   Creates a table consisting of the characters from the given string and the
   amount of times each character occurs in the given string
   PRECONDITION: s is sorted
   RETURNS:  A table that maps each character that occurs in s to the number of
            times the character occurs in s
   EXAMPLE: characterCountsAux "hej" 1 = T [('j',1),('e',1),('h',1)]
  -}
  characterCountsAux :: String -> Int -> Table Char Int
  characterCountsAux [] _ = Table.empty
  characterCountsAux [x] n = Table.insert (Table.empty) x n
  characterCountsAux (x:y:xs) n 
    | x == y = characterCountsAux (y:xs) (n+1)
    | otherwise = Table.insert (characterCountsAux (y:xs) 1) x n

-- bug, den plusar ej på 1 om inte två karaktärer som är lika kommer direkt efter varandra
-- juste, så de är precondition då, eftersom jag har konstruerat funktionen utifrån antagandet att strängen är sorterad

{-
INVARIANT: sub-trees with larger character counts do not occur at a lower level of the tree than
sub-trees with smaller character counts.
-}
data HuffmanTree = Leaf Char Int | Node (HuffmanTree) Int (HuffmanTree) deriving Show


{- huffmanTree t
   PRE:  t maps each key to a positive value, t is not empty
   RETURNS: a Huffman tree based on the character counts in t
   EXAMPLES: huffmanTree (characterCounts "hello there") =
   Node (Node (Leaf 'l' 2) 4 (Leaf 'h' 2)) 11 (Node (Leaf 'e' 3) 7 (Node (Node (   Leaf ' ' 1) 2 (Leaf 'r' 1)) 4 (Node (Leaf 't' 1) 2 (Leaf 'o' 1))))
 -}
huffmanTree :: Table Char Int -> HuffmanTree
huffmanTree t  = huffmanTreeAux (toPriorityQueue t) where
    --huffmanTreeAux q
    --performs the insertion algorithm for huffman trees based on a the priorityqueue q
    --VARIANT: length q
    huffmanTreeAux :: PriorityQueue HuffmanTree -> HuffmanTree
    huffmanTreeAux q
      | PriorityQueue.is_empty q1 = v1
      | otherwise = let ((v2,p2),q2) = least q1 in huffmanTreeAux $ PriorityQueue.insert (q2) (Node v1 (p1+p2) v2,(p1+p2))
      where
        ((v1,p1),q1) = least q
          

{- toPriorityQueue t
   Creates a priorityqueue of huffmantree leafs based on a character count.
   RETURNS: the huffmantrees associated with table t inserted as leafs into a priorityqueue
-}
toPriorityQueue :: Table Char Int -> PriorityQueue HuffmanTree
toPriorityQueue t =
  let
    --trivial
    buildTrees :: [HuffmanTree] -> (Char,Int) -> [HuffmanTree]
    buildTrees ts (c,n) = (Leaf c n) : ts
  in
    toPriorityQueueAux (Table.iterate t buildTrees [])
  where
    --toPriorityQueueAux hs
    --VARIANT: length hs
    toPriorityQueueAux :: [HuffmanTree] -> PriorityQueue HuffmanTree
    toPriorityQueueAux [] = PriorityQueue.empty
    toPriorityQueueAux (t@(Leaf _ n):ts) = PriorityQueue.insert (toPriorityQueueAux ts) (t,n)
    toPriorityQueueAux (t@(Node _ n _):ts) = PriorityQueue.insert (toPriorityQueueAux ts) (t,n)

{- codeTable h
   Takes given Huffman tree and makes a table consisting of the characters in
   the tree and their associated BitCode
   RETURNS: a table that maps each character in h to its Huffman code
   EXAMPLES: codeTable (Node (Leaf 'h' 1) 3 (Node (Leaf 'j' 1) 2 (Leaf 'e' 1))) = T [('e',[True,True]),('j',[True,False]),('h',[False])]
 -}
codeTable :: HuffmanTree -> Table Char BitCode
codeTable (Leaf _ _) = Table.empty
codeTable h = codeTableAcc h [] where
   {- codeTableAcc h bc
   Takes given Huffman tree and BitCode and creates a table consisting of the
   characters in the tree and their associated BitCode
   RETURNS: A table containing characters with associated BitCode.
   VARIANT: height h
   EXAMPLE: codeTableAcc ((Leaf 'h' 1) 3 (Node (Leaf 'j' 1) 2 (Leaf 'e' 1))) []
            = T [('e',[True,True]),('j',[True,False]),('h',[False])]
   -}
  codeTableAcc :: HuffmanTree -> BitCode -> Table Char BitCode
  codeTableAcc (Leaf c n) bc = Table.insert (Table.empty) c bc
  codeTableAcc (Node l _ r) bc =
    mergeTables x xs
    where
      x = codeTableAcc l (bc ++ [False])
      xs = codeTableAcc r (bc ++ [True])
  
{- mergeTables t1 t2
   Merges two tables into one.
   RETURNS: A merged table.
-}
mergeTables :: Eq a => Table a b -> Table a b -> Table a b
mergeTables t1 t2 =
  let
    insertElement :: Eq a => Table a b -> (a,b) -> Table a b
    insertElement ts e = Table.insert ts (fst e) (snd e)
  in
    Table.iterate t1 insertElement t2

{- bitCode s t
   Takes a given string and table and returns the BitCode associated with each
   character
   PRE: The amount of characters and each characters occurence in the string
        matches the table
   VARIANT: length s
   RETURNS: A list of bools / alternativt RETURNS: the BitCode associated to the string s given the code table t 
-}
bitCode :: String -> Table Char BitCode -> BitCode
bitCode [] t = []
bitCode (x:xs) t = bc ++ bitCode xs t 
  where bc = case Table.lookup t x of Just x -> x


{- compress s
   Takes a string and returns a tuple consisting of the Huffman tree created
   from the string and the BitCode associated to each character
   RETURNS: A Huffman tree based on s, the Huffman coding of s under this tree
   EXAMPLES: compress "hej" = (Node (Leaf 'h' 1) 3 (Node (Leaf 'j' 1) 2 (Leaf
             'e' 1)),[False,True,True,True,False])
 -}
compress :: String -> (HuffmanTree, BitCode) 
compress "" = (Leaf ' ' 0, [])
compress b =
  let
    cc = characterCounts
    ht = huffmanTree
    ct = codeTable
  in
    (ht (cc b), bitCode b (ct(ht(cc b))))

{- decompress h bits
   Decodes the given BitCode that is associated to the given Huffman tree
   PRE:  Bits is a concatenation of valid Huffman code words for h
   RETURNS: The decoding of bits under h
   EXAMPLES: decompress (Node (Leaf 'h' 1) 3 (Node (Leaf 'j' 1) 2 (Leaf 'e' 1))) ([False,True,True,True,False]) = "hej"
 -}
decompress :: HuffmanTree -> BitCode -> String
decompress (Leaf c n) _ = replicate n c
decompress _ [] = []
decompress t bc =
  let (c,bits) = decompressAux t bc in
    c : (decompress t bits) where
    {- decompressAux t bc
    Recursively goes through the given Huffman tree and decode every character.
    RETURNS: A tuple containing a character and its' associated BitCode.
    VARIANT: length bc
    -}
    decompressAux :: HuffmanTree  -> BitCode -> (Char, BitCode)
    decompressAux (Leaf c _) [] = (c, [])
    decompressAux (Leaf c _) bits = (c,bits)
    decompressAux (Node l _ r) (bit:bits)
      | bit == True = decompressAux r bits
      | bit == False = decompressAux l bits

--------------------------------------------------------------------------------
-- Test Cases
-- You may add your own test cases here:
-- Follow the pattern and/or read about HUnit on the interwebs.
--------------------------------------------------------------------------------

-- characterCounts
test1 = TestCase $ assertEqual "characterCounts"
            (Just 7) (Table.lookup (characterCounts "this is an example of a huffman tree") ' ')

-- codeTable
-- while the precise code for ' ' may vary, its length (for the given example string) should always be 3 bits
test2 = TestCase $ assertEqual "codeTable"
            3 (maybe (-1) length (Table.lookup (codeTable (huffmanTree (characterCounts "this is an example of a huffman tree"))) ' '))

-- compress
-- while the precise code for the given example string may vary, its length should always be 135 bits
test3 = TestCase $ assertEqual "compress"
            135 (length (snd (compress "this is an example of a huffman tree")))

-- decompress
test4 =
    let s = "this is an example of a huffman tree"
    in
      TestCase $ assertEqual ("decompress \"" ++ s ++ "\"")
        s (let (h, bits) = compress s in decompress h bits)

test5 =
    let s = "xxx"
    in
      TestCase $ assertEqual ("decompress \"" ++ s ++ "\"")
        s (let (h, bits) = compress s in decompress h bits)

test6 =
    let s = ""
    in
      TestCase $ assertEqual ("decompress \"" ++ s ++ "\"")
        s (let (h, bits) = compress s in decompress h bits)

-- for running all the tests
runtests = runTestTT $ TestList [test1, test2, test3, test4, test5, test6]
