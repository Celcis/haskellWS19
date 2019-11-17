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
isNode = undefined

-- | Mache alle Knoten im Baum, die wie __/Node element Empty Empty/__ aussehen, so: __/Leaf element/__
makeUnhollow :: Tree a -> Tree a
makeUnhollow = undefined

-- | gibt das linke Kind
leftChild :: Tree a -> Maybe(Tree a)
leftChild = undefined

-- | gibt das rechte Kind
rightChild :: Tree a -> Maybe(Tree a)
rightChild = undefined

-- | Anzahl der Knoten und Blätter
nodeCount :: Tree a -> Int
nodeCount = undefined

-- | Finde den kleinsten Index
min :: Tree a -> Maybe a
min = undefined

-- | gibt den größten Index im Baum
max :: (Eq a, Ord a) => Tree a -> Maybe a
max = undefined

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
