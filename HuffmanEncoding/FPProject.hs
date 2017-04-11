import Data.Tree
import Data.Tree.Pretty
import Data.List
import Data.Function

freqTable :: Ord a => [a] -> [(a, Int)]
freqTable str = map (\x->(head x, length x))  (group (sort str))

freqTableBy :: Ord a => (a -> a -> Bool) -> [a] -> [([a], Int)]
freqTableBy fun str  = map (\x->([head x], length x))  (groupBy fun (sort str))

sortedFreqTable :: Ord a0 => [(a,a0)] -> [(a,a0)]
sortedFreqTable = sortBy (compare `on` snd)

data HuffmanTree  = Leaf Char Int
             | ParentNode HuffmanTree HuffmanTree Int deriving (Show)

freq :: HuffmanTree -> Int             
freq (Leaf _ num) = num
freq (ParentNode _ _ sumChildren) = sumChildren

mergeNodes :: HuffmanTree -> HuffmanTree -> HuffmanTree
mergeNodes node1 node2 = ParentNode node1 node2 (freq node1 + freq node2)

buildTree :: [(Char, Int)] -> HuffmanTree
buildTree = build . map (\(ch,fr) -> Leaf ch fr) . sortedFreqTable
    where  build (root:[])    = root
           build (child1:child2:rest)  = build $ insertBy (compare `on` freq) (mergeNodes child1 child2) rest

inputToTree :: String -> HuffmanTree
inputToTree str = buildTree $ freqTable str

buildCodeDict :: String -> HuffmanTree -> [(Char, String)]
buildCodeDict s (Leaf ch _) = [(ch,s)]
buildCodeDict s (ParentNode left right _) = buildCodeDict (s ++ "0") left
                                  ++ buildCodeDict (s ++ "1") right

encode :: String -> String
encode str = concat $ map (\[(e1,e2)] -> e2) $ map (\ch -> filter (\(x,y) -> x == ch) (buildCodeDict "" (inputToTree str))) str

--convert HuffmanTree to Data.Tree so it could be printed nicely
toTree :: HuffmanTree -> Tree String
toTree (Leaf label _) = Node [label] []
toTree (ParentNode left right sumChildren) = Node (show sumChildren)[toTree left, toTree right] 

decode :: HuffmanTree -> String -> String
decode tree [] = []
decode tree code = trav code tree
     where trav rest (Leaf x _) = x : decode tree rest
           trav ('0':rest) (ParentNode l _ _) = trav rest l
           trav ('1':rest) (ParentNode _ r _) = trav rest r

main = do x <- readFile "toencode.txt"
	  putStrLn $ drawVerticalTree $ toTree (inputToTree x)
	  let s = encode x
	  writeFile "encoded.txt" s
	  y <- readFile "encoded.txt"
	  putStrLn $ decode (inputToTree x) y