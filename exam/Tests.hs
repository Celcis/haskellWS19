module Tests where

import Text.Printf

import Test.Tasty
import Test.Tasty.HUnit

import ElimEvensDoubleOdds
import Plot

run = defaultMain $ testGroup "Programmieraufgaben Übungsklausur" [
    exercise1,
    exercise2 
    ]

exercise :: Int-> String -> Int -> [TestTree] -> TestTree
exercise i nm pts = testGroup (printf "%d %-40s (%d Punkt(e))" i nm pts)

exercise1 = exercise 1 "ElimEvensDoubleOdds" 1 $ [
  testCase "elimEvensDoubleOdds [1,2,3]" $
     elimEvensDoubleOdds [1,2,3] @?= [2,6],
  testCase "elimEvensDoubleOdds [2,4,6]" $ 
     elimEvensDoubleOdds [2,4,6] @?= []
  ]

exercise2 = exercise 2 "Plot" 3 $ [
  testCase "plot id [1..3] 3 '+'" $
     plot id [1..3] 3 '+' @?= "1.0   +\n2.0      +\n3.0         +\n",
  testCase "plot (\\x -> -(x^2)+4) [0..2] 2 'X'" $
     plot (\x -> -(x^2)+4) [0..2] 2 'X' @?= "0.0        X\n1.0      X\n2.0X\n"
  ]
