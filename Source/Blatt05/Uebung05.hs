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
makeUnhollow Empty = Empty
makeUnhollow (Node a Empty Empty ) = Leaf a 


-- | gibt das linke Kind
leftChild :: Tree a -> Maybe(Tree a)
leftChild Empty = Nothing
leftChild (Leaf a) = Nothing
leftChild (Node _ l _ ) = leftChild l 


-- | gibt das rechte Kind
rightChild :: Tree a -> Maybe(Tree a)
rightChild Empty = Nothing
rightChild (Leaf a) = Nothing
rightChild (Node _ _ r) = Just r

-- | Anzahl der Knoten und Blätter
nodeCount :: Tree a -> Int
nodeCount Empty = 0
nodeCount (Leaf a) = 1
nodeCount (Node a left right) = nodeCount left + nodeCount right + 1

-- | Finde den kleinsten Index
min :: Tree a -> Maybe a
min Empty = Nothing
min (Leaf a) = Just a
min (Node a Empty right ) = Just a
min (Node _ left _ ) = min left

-- | gibt den größten Index im Baum
max :: (Eq a, Ord a) => Tree a -> Maybe a
max Empty = Nothing 
max (Leaf a) = Just a
max (Node x _ Empty )= Just x
max (Node x _ r) = max r

-- eine helperfunktion fur insert
whenTreeEmpty:: a -> Tree a 
whenTreeEmpty n = Leaf n


-- | Einfügen Elemente in die Liste
insert :: (Ord a) => Tree a -> a -> Tree a
insert Empty n = whenTreeEmpty n
insert (Leaf a) n = Node a Empty (Leaf n) 
insert (Node a left right) n
    | n == a = Node n left right
    | n < a  = Node a (Leaf n) right
    | n > a  = Node a left (insert right n)


-- | helperfunktion, die ein Baum als Liste darstellt 
toList :: (Ord a) => Tree a -> [a]
toList Empty = []
toList (Node v t1  t2) = toList t1 ++ [v] ++ toList t2


-- | Ob alle Knoten im Baum das eingegebene Kriterium bestimmt
fulfillTree :: (a -> Bool) -> Tree a -> Bool
fulfillTree f Empty = True
fulfillTree f (Leaf a) = f a
fulfillTree f (Node a left right) =  f a && fulfillTree f left && fulfillTree f right 
    


-- | Gibt alle Elemente des Baums, die das eingegebene Kriterium bestimmen
filterTreeToList :: (a -> Bool) -> Tree a -> [a]
filterTreeToList = undefined

-- | Gibt die Anzahl der Knoten mit dem angegebenen Schluessel
countElem :: (Eq a) => Tree a -> a -> Int
countElem Empty _ = 0
countElem (Leaf a) n = if a == n then 1 else 0
countElem (Node a left right ) n
    | a == n = 1
    | True = countElem right n + countElem left n



-- | helperfunktion, die ein Baum als Liste darstellt 
toList :: Tree a -> [a]
toList Empty = []
toList (Leaf a) = [a]
toList (Node a t1  t2) = toList t1 ++ [a] ++ toList t2

-- | diese Funktion loescht dublicate Elemente aus der List
dropNub :: (Eq a) => [a] -> [a]
dropNub [] = []
dropNub (x:xs) = x : dropNub (filter (/=x) xs)


-- | Ob der Baum Duplikate hat.
hasDuplicates :: ( Eq a) => Tree a -> Bool
hasDuplicates Empty = False
hasDuplicates (Leaf a) = False
hasDuplicates baum
    | (length $ dropNub $ toList (baum)) == (length $ toList (baum)) = False
    | True = True
   
-- ==============================================================================================

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

