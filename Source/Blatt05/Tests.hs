{-|
Module      : Tests
Description : This is the Tests module for the module Uebung05
Copyright   : _
License     : _
Maintainer  : soliman@uni-bremen.de
Stability   : experimental
Portability : _

Has the tests
-}

module Tests where

import Prelude hiding (min,max)
import Uebung05
import Data.List (sort , (\\))
import Test.Tasty
import Test.Tasty.HUnit

-- | tree1
tree1 :: (Eq a,Num a) => Tree a
tree1 = Leaf 5

-- | tree2
tree2 :: (Eq a,Num a) => Tree a
tree2 = Node 5 Empty (Leaf 7)

-- | unsortedTree2
unsortedTree2 :: (Eq a,Num a) => Tree a
unsortedTree2 = Node 5 (Leaf 7) Empty

-- | tree3
tree3 :: (Eq a,Num a) => Tree a
tree3 = Node 5 (Leaf 4) (Leaf 7)

-- | tree4
tree4 :: (Eq a,Num a) => Tree a
tree4 = Node 5 (Leaf 4) (Leaf 6)

-- | tree5
tree5 :: (Eq a,Num a) => Tree a
tree5 = Node 5 (Leaf 4) (Node 7 (Leaf 6) Empty)
-----------------------------------------------
-- | tree6
tree6 :: (Eq a,Num a) => Tree a
tree6 = Node 6 (Node 4 (Leaf 3) Empty) (Leaf 7)

-- | unsortedTree6
unsortedTree6 :: (Eq a,Num a) => Tree a
unsortedTree6 = Node 6 (Node 4 Empty (Leaf 3)) (Leaf 7)

-- | tree7
tree7 :: (Eq a,Num a) => Tree a
tree7 = Node 6 (Node 4 (Leaf 3) (Leaf 5)) (Leaf 7)

-- | tree8
tree8 :: (Eq a,Num a) => Tree a
tree8 = Node 10 (Node 9 (Leaf 8) (Leaf 15)) (Node 11 (Leaf 7) Empty)

-- | tree9
tree9 :: (Eq a,Num a) => Tree a
tree9 = Node 10 (Node 9 (Leaf 8) (Leaf 15)) (Node 11 (Leaf 7) (Leaf 12))

-- | tree10
tree10 :: (Eq a,Num a) => Tree a
tree10 = Node 10 (Node 9 (Node 8 (Node 6 (Node 4 (Leaf 3) Empty) (Leaf 7)) Empty) (Leaf 15)) (Node 11 (Leaf 7) Empty)

-- | tree11
tree11 :: (Eq a,Num a) => Tree a
tree11 = Node 6 (Node 4 (Leaf 3) Empty) (Node 7 Empty (Node 10 (Node 9 (Leaf 8) (Leaf 15)) (Node 11 (Leaf 7) Empty)))

-- | tree12
tree12 :: (Eq a, Num a) => Tree a
tree12 = Node 6 (Node 2 (Leaf 1) Empty) (Node 11 (Node 8 Empty (Node 9 (Leaf 10) Empty)) (Leaf 15))

-- | treeWithDuplicates
treeWithDuplicates :: (Eq a, Num a) => Tree a
treeWithDuplicates = Node 6 (Node 5 (Leaf 4) (Leaf 5)) (Node 7 (Leaf 6) (Leaf 8))

-- | run
run = defaultMain $ testGroup "All tests" [isEmptyTests , isLeafTests , isNodeTests , makeUnhollowTests , leftChildTests , rightChildTests , minTests , maxTests , nodeCountTests , insertTests , fulfillTreeTests , filterTreeToListTests , countElemTests , hasDuplicatesTests]

-- | isEmptyTests
isEmptyTests :: TestTree
isEmptyTests = testGroup "<<<<<<<<<<<<<<<<<<<< Test for isEmpty >>>>>>>>>>>>>>>>>>>>"
  [ testCase "Test01" $
    assertEqual "wrong result" (True) (isEmpty Empty)
  ,
    testCase "Test02" $
    assertEqual "wrong result" (False) (isEmpty tree1)
  ]

-- | isLeafTests
isLeafTests :: TestTree
isLeafTests = testGroup "<<<<<<<<<<<<<<<<<<<< Test for isLeaf >>>>>>>>>>>>>>>>>>>>"
  [ testCase "Test01" $
    assertEqual "wrong result" (False) (isLeaf Empty)
  ,
    testCase "Test02" $
    assertEqual "wrong result" (True) (isLeaf tree1)
  ,
    testCase "Test03" $
    assertEqual "wrong result" (False) (isLeaf tree2)
  ]

-- | isNodeTests
isNodeTests :: TestTree
isNodeTests = testGroup "<<<<<<<<<<<<<<<<<<<< Test for isNode >>>>>>>>>>>>>>>>>>>>"
  [ testCase "Test01" $
    assertEqual "wrong result" (False) (isNode Empty)
  ,
    testCase "Test02" $
    assertEqual "wrong result" (False) (isNode tree1)
  ,
    testCase "Test03" $
    assertEqual "wrong result" (True) (isNode tree2)
  ,
    testCase "Test04" $
    assertEqual "wrong result" (True) (isNode (Node 4 Empty Empty))
  ]

-- | makeUnhollowTests
makeUnhollowTests :: TestTree
makeUnhollowTests = testGroup "<<<<<<<<<<<<<<<<<<<< Test for makeUnhollow >>>>>>>>>>>>>>>>>>>>"
  [ testCase "Test01" $
    assertEqual "wrong result" (Leaf 5) (makeUnhollow  (Node 5 Empty Empty))
  ,
    testCase "Test02" $
    assertEqual "wrong result" (Node 6 (Leaf 5) (Leaf 7)) (makeUnhollow (Node 6 (Node 5 Empty Empty) (Node 7 Empty Empty)))
  ,
    testCase "Test03" $
    assertEqual "wrong result" (Empty::Tree Int) (makeUnhollow (Empty::Tree Int))
  ]

-- | leftChildTests
leftChildTests :: TestTree
leftChildTests = testGroup "<<<<<<<<<<<<<<<<<<<< Test for leftChild >>>>>>>>>>>>>>>>>>>>"
  [ testCase "Test01" $
    assertEqual "wrong result" (Nothing) (leftChild (Empty::Tree Int))
  ,
    testCase "Test02" $
    assertEqual "wrong result" (Nothing) (leftChild tree1)
  ,
    testCase "Test03" $
    assertEqual "wrong result" (Nothing) (leftChild tree2)
  ,
    testCase "Test04" $
    assertEqual "wrong result" (Just (Leaf 4)) (leftChild tree4)
  ]

-- | rightChildTests
rightChildTests :: TestTree
rightChildTests = testGroup "<<<<<<<<<<<<<<<<<<<< Test for leftChild >>>>>>>>>>>>>>>>>>>>"
  [ testCase "Test01" $
    assertEqual "wrong result" (Nothing) (rightChild (Empty::Tree Int))
  , testCase "Test02" $
    assertEqual "wrong result" (Nothing) (rightChild tree1)
  ,
    testCase "Test03" $
    assertEqual "wrong result" (Just (Leaf 7::Tree Int)) (rightChild tree3)
  ,
    testCase "Test04" $
    assertEqual "wrong result" (Just (Node 7 (Leaf 6) Empty)) (rightChild tree5)
  ]

-- | minTests
minTests :: TestTree
minTests = testGroup "<<<<<<<<<<<<<<<<<<<< Test for min >>>>>>>>>>>>>>>>>>>>"
  [ testCase "Test01" $
    assertEqual "wrong result" (Nothing) (min (Empty::Tree Int))
  ,
    testCase "Test02" $
    assertEqual "wrong result" (Just 5) (min (Leaf 5))
  ,
    testCase "Test03" $
    assertEqual "wrong result" (Just 5) (min tree2)
  ,
    testCase "Test04" $
    assertEqual "wrong result" (Just 4) (min tree3)
  ,
    testCase "Test05" $
    assertEqual "wrong result" (Just 5) (min (Node 6 (Leaf 5) (Leaf 7)))
  ,
    testCase "Test06" $
    assertEqual "wrong result" (Just 4) (min (Node 7 (Node 5 (Leaf 4) (Leaf 6)) (Leaf 8)))
  ,
    testCase "Test07" $
    assertEqual "wrong result" (Just 5) (min (Node 7 (Node 5 Empty (Leaf 6)) (Leaf 8)))
  ,
    testCase "Test08" $
    assertEqual "wrong result" (Just 7) (min (Node 7 Empty (Leaf 8)))
  ,
    testCase "Test09" $
    assertEqual "wrong result" (Just 7) (min (Node 7 Empty (Node 8 Empty (Leaf 9))))
  ,
    testCase "Test10" $
    assertEqual "wrong result" (Just 8) (min (Node 11 (Node 8 Empty (Node 9 Empty (Leaf 10))) (Leaf 15)))
  ]

-- | maxTests
maxTests :: TestTree
maxTests = testGroup "<<<<<<<<<<<<<<<<<<<< Test for max >>>>>>>>>>>>>>>>>>>>"
  [ testCase "Test01" $
    assertEqual "wrong result" (Nothing) (max (Empty::Tree Int))
  ,
    testCase "Test02" $
    assertEqual "wrong result" (Just 5) (max (Leaf 5))
  ,
    testCase "Test03" $
    assertEqual "wrong result" (Just 7) (max tree2)
  ,
    testCase "Test04" $
    assertEqual "wrong result" (Just 7) (max tree3)
  ,
    testCase "Test05" $
    assertEqual "wrong result" (Just 7) (max (Node 6 (Leaf 5) (Leaf 7)))
  ,
    testCase "Test06" $
    assertEqual "wrong result" (Just 8) (max (Node 7 (Node 5 (Leaf 4) (Leaf 6)) (Leaf 8)))
  ,
    testCase "Test07" $
    assertEqual "wrong result" (Just 8) (max (Node 7 (Node 5 Empty (Leaf 6)) (Leaf 8)))
  ,
    testCase "Test08" $
    assertEqual "wrong result" (Just 8) (max (Node 7 Empty (Leaf 8)))
  ,
    testCase "Test09" $
    assertEqual "wrong result" (Just 9) (max (Node 7 Empty (Node 8 Empty (Leaf 9))))
  ,
    testCase "Test10" $
    assertEqual "wrong result" (Just 15) (max (Node 11 (Node 8 Empty (Node 9 Empty (Leaf 10))) (Leaf 15)))
  ]

-- | nodeCountTests
nodeCountTests :: TestTree
nodeCountTests = testGroup "<<<<<<<<<<<<<<<<<<<< Test for nodeCount >>>>>>>>>>>>>>>>>>>>"
  [ testCase "Test01" $
    assertEqual "wrong result" (0) (nodeCount Empty)
  , 
    testCase "Test02" $
    assertEqual "wrong result" (1) (nodeCount tree1)
  ,
    testCase "Test03" $
    assertEqual "wrong result" (2) (nodeCount tree2)
  ,
    testCase "Test04" $
    assertEqual "wrong result" (3) (nodeCount tree3)
  ,
    testCase "Test05" $
    assertEqual "wrong result" (4) (nodeCount tree5)
  ]

-- | insertTests
insertTests :: TestTree
insertTests = testGroup "<<<<<<<<<<<<<<<<<<<< Test for insert >>>>>>>>>>>>>>>>>>>>"
  [ testCase "Test01" $
    assertEqual "wrong result" (Leaf 5) (insert Empty 5)
  ,
    testCase "Test02" $
    assertEqual "wrong result" (tree2) (insert tree1 7)
  ,
    testCase "Test03" $
    assertEqual "wrong result" (tree3) (insert tree3 4)
 ]

-- | fulfillTreeTests
fulfillTreeTests :: TestTree
fulfillTreeTests = testGroup "<<<<<<<<<<<<<<<<<<<< Test for fulfillTreeTests >>>>>>>>>>>>>>>>>>>>"
  [ testCase "Test01" $
    assertEqual "wrong result" (True) (fulfillTree odd tree2)
  , testCase "Test02" $
    assertEqual "wrong result" (False) (fulfillTree even tree2)
  , testCase "Test03" $
    assertEqual "wrong result" (True) (fulfillTree (>=4) tree4)
  , testCase "Test04" $
    assertEqual "wrong result" (False) (fulfillTree (>=5) tree4)
  , testCase "Test05" $
    assertEqual "wrong result" (True) (fulfillTree (==0) Empty)
  ]

-- | filterTreeToListTests
filterTreeToListTests :: TestTree
filterTreeToListTests = testGroup "<<<<<<<<<<<<<<<<<<<< Test for filterTreeToListTests >>>>>>>>>>>>>>>>>>>>"
  [ testCase "Test01" $
    assertEqual "wrong result" ([4,6]) (sort $ filterTreeToList even tree4)
  , testCase "Test02" $
    assertEqual "wrong result" ([5]) (sort $ filterTreeToList odd tree4)
  , testCase "Test03" $
    assertEqual "wrong result" ([4,5,6]) (sort $ filterTreeToList (>=4) tree4)
  , testCase "Test04" $
    assertEqual "wrong result" ([5,6]) (sort $ filterTreeToList (>=5) tree4)
  , testCase "Test05" $
    assertEqual "wrong result" ([]) (filterTreeToList (==0) Empty)
  ]

-- | countElemTests
countElemTests ::TestTree
countElemTests = testGroup "<<<<<<<<<<<<<<<<<<<< Test for countElem >>>>>>>>>>>>>>>>>>>>"
  [ testCase "Test01" $
    assertEqual "wrong result" (1) (countElem (tree1) 5)
  ,
    testCase "Test02" $
    assertEqual "wrong result" (1) (countElem (Node 5 Empty (Leaf 7)) 5)
  ,
    testCase "Test03" $
    assertEqual "wrong result" (2) (countElem tree10 7)
  ]

-- | hasDuplicatesTests
hasDuplicatesTests :: TestTree
hasDuplicatesTests = testGroup "<<<<<<<<<<<<<<<<<<<< Test for hasDuplicates >>>>>>>>>>>>>>>>>>>>"
  [ testCase "Test01" $
    assertEqual "wrong result" (False) (hasDuplicates tree4)
  , testCase "Test02" $
    assertEqual "wrong result" (False) (hasDuplicates tree9)
  , testCase "Test03" $
    assertEqual "wrong result" (True) (hasDuplicates tree10)
  ]
