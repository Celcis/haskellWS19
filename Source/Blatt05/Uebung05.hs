module Uebung05 where
import Prelude hiding (min,max)
import qualified Data.Tree as T hiding (Tree)


-- | Definieren des Baums
data Tree a = Node a (Tree a) (Tree a) | Leaf a | Empty 
              deriving (Ord, Eq, Show)

-- | Ob der Baum leer ist.
isEmpty :: Tree a -> Bool
isEmpty Empty = True
isEmpty _ = False

-- | Ob der eingegebene Knoten ein Blatt ist.
isLeaf :: Tree a -> Bool
isLeaf (Leaf a) = True
isLeaf _ = False

-- | ob der eingegebene Knoten Kinder hat
isNode :: Tree a -> Bool
isNode Empty = False
isNode (Leaf a) = False
isNode _ = True

-- =========================================================================
-- | Mache alle Knoten im Baum, die wie __/Node element Empty Empty/__ aussehen, so: __/Leaf element/__
makeUnhollow :: Tree a -> Tree a
makeUnhollow = undefined

-- | gibt das linke Kind
leftChild :: Tree a -> Maybe(Tree a)
leftChild Empty = Nothing
leftChild (Node a left Empty ) = Just left
leftChild (Node a left right) = leftChild left


-- | gibt das rechte Kind
rightChild :: Tree a -> Maybe(Tree a)
rightChild Empty = Nothing
rightChild (Node a Empty right ) = Just right
rightChild (Node a left right) = rightChild right

-- | Anzahl der Knoten und Blätter
nodeCount :: Tree a -> Int
nodeCount Empty = 0
nodeCount (Leaf a) = 1
nodeCount (Node a left right) = nodeCount left + nodeCount right + 1

-- | Finde den kleinsten Index
min :: Tree a -> Maybe a
min Empty = Nothing
min (Node a Empty right ) = Just a
min (Node _ left _ ) = min left

-- | gibt den größten Index im Baum
max :: (Eq a, Ord a) => Tree a -> Maybe a
max Empty = Nothing 
max (Node a left right )
    | left > right = max left 
    | otherwise = max right

-- | Einfügen Elemente in die Liste
insert :: (Ord a) => Tree a -> a -> Tree a
insert = undefined

-- | Ob alle Knoten im Baum das eingegebene Kriterium bestimmt
fulfillTree :: (a -> Bool) -> Tree a -> Bool
fulfillTree = undefined

-- | Gibt alle Elemente des Baums, die das eingegebene Kriterium bestimmen
filterTreeToList :: (a -> Bool) -> Tree a -> [a]
filterTreeToList = undefined

-- | Gibt die Anzahl der Knoten mit dem angegebenen Schluessel
countElem :: (Eq a) => Tree a -> a -> Int
countElem = undefined

-- | Ob der Baum Duplikate hat.
hasDuplicates :: (Eq a) => Tree a -> Bool
hasDuplicates = undefined

-- | Hilfsmethode für die main-Methode zum Ascii-Zeichnen des Baums.
--   Wandelt der oben definierte Baum in den in 'Data.Tree' definierten Baum um
toDataTree (Leaf a) = T.Node a []
toDataTree (Node b left right) = T.Node b [toDataTree left, toDataTree right]

-- | Hilfsmethode für die main-Methode zum Ascii-Zeichnen des Baums.
--   Konvertiert den Baum zum String-Baum
printableTree :: Tree Integer -> Tree String
printableTree Empty = Leaf ""
printableTree (Leaf v) = Leaf $ show $ fromInteger v
printableTree (Node v node1 node2) = Node (show $ fromInteger v) (printableTree node2) (printableTree node1)

-- | Die main-Methode.
--   Gib am Ende der Implementierung der main-Methode den zu zeichnenden Baum
printTree :: Tree Integer -> IO()
printTree tree = putStrLn $ T.drawTree $ toDataTree $ printableTree tree
