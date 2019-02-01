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

--Trivial
insertionSort xs = insertionSortAux [] xs
insertionSortAux :: Ord a => [a] -> [a] -> [a]
insertionSortAux sorted [] = sorted
insertionSortAux sorted (x:xs) = insertionSortAux (insert1 x sorted) xs

insert1 :: Ord a => a -> [a] -> [a]
insert1 elem [] = [elem]
insert1 elem (x:xs) 
  | elem < x = elem:x:xs
  | otherwise = x:(insert1 elem xs)

{- characterCounts s
   RETURNS: a table that maps each character that occurs in s to the number of
         times the character occurs in s
   EXAMPLES:
 -}
characterCounts :: String -> Table Char Int
characterCounts s = characterCountsAux (insertionSort s) 1

characterCountsAux :: String -> Int -> Table Char Int
characterCountsAux [] _ = Table.empty
characterCountsAux [x] n = Table.insert (Table.empty) x n
characterCountsAux (x:y:xs) n 
  | x == y = characterCountsAux (y:xs) (n+1)
  | otherwise = Table.insert (characterCountsAux (y:xs) 1) x n


-- modify and add comments as needed
{-
INVARIANT: sub-trees with larger character counts do not occur at a lower level of the tree than
sub-trees with smaller character counts.
-}
data HuffmanTree = Leaf Char Int | Node (HuffmanTree) Int (HuffmanTree) deriving Show

{- huffmanTree t
   PRE:  t maps each key to a positive value
   RETURNS: a Huffman tree based on the character counts in t
   EXAMPLES:
 -}
huffmanTree :: Table Char Int -> HuffmanTree
huffmanTree t = huffmanTreeAux (toPriorityQueue t)

huffmanTreeAux :: PriorityQueue HuffmanTree -> HuffmanTree
huffmanTreeAux q
  | PriorityQueue.is_empty q1 = v1
  | otherwise = huffmanTreeAux $ PriorityQueue.insert (q2) (Node v1 (p1+p2) v2,(p1+p2))
  where
    ((v1,p1),q1) = least q
    ((v2,p2),q2) = least q1

toPriorityQueue :: Table Char Int -> PriorityQueue HuffmanTree
toPriorityQueue t =
  let
    buildTrees :: [HuffmanTree] -> (Char,Int) -> [HuffmanTree]
    buildTrees ts (c,n) = (Leaf c n) : ts
  in
    toPriorityQueueAux (Table.iterate t buildTrees [])

toPriorityQueueAux :: [HuffmanTree] -> PriorityQueue HuffmanTree
toPriorityQueueAux [] = PriorityQueue.empty
toPriorityQueueAux (t@(Leaf _ n):ts) = PriorityQueue.insert (toPriorityQueueAux ts) (t,n)
toPriorityQueueAux (t@(Node _ n _):ts) = PriorityQueue.insert (toPriorityQueueAux ts) (t,n)

--TESTING
x = characterCounts "this is an example of a huffman tree"
h = huffmanTree x
ct = codeTable h
b = "this is an example of a huffman tree"
{- codeTable h
   RETURNS: a table that maps each character in h to its Huffman code
   EXAMPLES:
      codeTable Node (Leaf 'h' 1) 3 (Node (Leaf 'j' 1) 2 (Leaf 'e' 1)) =
      T [('e',[True,True]),('j',[True,False]),('h',[False])]
 -}
codeTable :: HuffmanTree -> Table Char BitCode
codeTable h = codeTableAcc h [] 

bitCode :: String -> Table Char BitCode -> BitCode
bitCode [] t = []
bitCode (x:xs) t = bc ++ bitCode xs t 
  where bc = case Table.lookup t x of Just x -> x
  

codeTableAcc :: HuffmanTree -> BitCode -> Table Char BitCode
codeTableAcc (Leaf c n) bc = Table.insert (Table.empty) c bc
codeTableAcc (Node l _ r) bc =
  mergeTables x xs
  where
    x = codeTableAcc l (bc ++ [False])
    xs = codeTableAcc r (bc ++ [True])

mergeTables :: Eq a => Table a b -> Table a b -> Table a b
mergeTables t1 t2 =
  let
    insertElement :: Eq a => Table a b -> (a,b) -> Table a b
    insertElement ts e = Table.insert ts (fst e) (snd e)
  in
    Table.iterate t1 insertElement t2

{- compress s
   RETURNS: (a Huffman tree based on s, the Huffman coding of s under this tree)
   EXAMPLES:
 -}
compress :: String -> (HuffmanTree, BitCode)
compress b =
  let
    cc = characterCounts
    ht = huffmanTree
    ct = codeTable
  in
    (huffmanTree (characterCounts b), bitCode b (ct(ht(cc b))))


{- decompress h bits
   PRE:  bits is a concatenation of valid Huffman code words for h
   RETURNS: the decoding of bits under h
   EXAMPLES:
 -}
decompress :: HuffmanTree -> BitCode -> String
decompress _ [] = []
decompress t bc =
  let (c,bits) = decompressAux t bc in
    c : (decompress t bits)

decompressAux :: HuffmanTree  -> BitCode -> (Char, BitCode)
decompressAux (Leaf c _) [] = (c, [])
decompressAux (Leaf c _) (bit:bits) = (c,(bit:bits))
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
