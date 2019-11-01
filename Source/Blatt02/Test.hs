module Test where

import Uebung_2

import Test.Tasty
import Test.Tasty.HUnit

run = defaultMain tests

tests = testGroup "Tests zu Uebungsblatt 2" 
  [ testCase "subStrOf \"Has\" \"Haskell ist toll\" == True" $
    assertEqual "wrong result" True (subStrOf "Has" "Haskell ist toll")
    , testCase "subStrOf \"oll\" \"Haskell ist toll\" == True" $
    assertEqual "wrong result" True (subStrOf "oll" "Haskell ist toll")
    , testCase "subStrOf \"skull\" \"Haskell ist toll\" == False" $
    assertEqual "wrong result" False (subStrOf "skull" "Haskell ist toll")
    , testCase "subStrOf \"badu\" \"Dabadidabodu\" == False" $
    assertEqual "wrong result" False (subStrOf "badu" "Dabadidabodu")
    , testCase "robber \"himbeereis\"" $
    assertEqual "wrong result" "hohimombobeeroreisos" (robber "himbeereis")
    , testCase "unrobber \"kokaesosetotoasostot\"" $
    assertEqual "wrong result" "kaesetoast" (unrobber "kokaesosetotoasostot")
    
    , testCase "subStrOf \"Das\" \"Das ist ein string\" == True" $
    assertEqual "wrong result" True (subStrOf "Das" "Das ist ein string"
    , testCase "subStrOf \"rs\" "Ders\" == True" $
    assertEqual "wrong result" True (subStrOf "rs" "Ders"
    , testCase "subStrOf \"tem\" \"Mehmet\" == False" $
    assertEqual "wrong result" False (subStrOf "tem" "Mehmet")
    , testCase "subStrOf \"blabla\" \"ablvasss\" == False" $
    assertEqual "wrong result" False (subStrOf "blabla" "ablvasss")
    , testCase "robber \"taksi\"" $
    assertEqual "wrong result" "totakoksosi" (robber "taksi")
    , testCase "unrobber \"hohesos\"" $
    assertEqual "wrong result" "hes" (unrobber "hohesos")

  ]
