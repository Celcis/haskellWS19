module Uebung_3 where


------------Aufgabe 1 ------------
-- | Die Funtion prüft, ob eingegebene Zahl eine Primezahl ist
isPrime :: Integer -> Bool
isPrime 2 = True 
isPrime n = n >  2 && []==[i | i <- [2..n-1], rem n i == 0]


-- | Die Funktion gibt liste von Primzahlen zwischen eingegebenen Zahlen
primesFromTo :: Integer -> Integer -> [Integer]
primesFromTo a b = [x | x <- [2..b], isPrime x && x > a]






-----------Aufgabe 2 -------------

keyChars :: [(Char,String)]
keyChars
   = [('1', " \n1")
     ,('2', "abc2")
     ,('3', "def3")
     ,('4', "ghi4")
     ,('5', "jkl5")
     ,('6', "mno6")
     ,('7', "pqrs7")
     ,('8', "tuv8")
     ,('9', "wxyz9")
     ,('0', ".,?!'\"0-()@/:_")
     ,('*', "*")
     ,('#', "#")
     ]

-- place determines the place of a character in a string
-- place 'a' ``abc'' ~~> 1 (not 0!)
-- unsafe in general, but simpler than  elemIndex or elemIndices (Data.List)

place :: Char -> String -> Int
place c [] = error (c:" does not occur in keyChars!")
place c (x:xs) 
  | x == c    = 1
  | otherwise = 1 + place c xs

charKeys :: [(Char,(Char,Int))]
charKeys
    = [('a',('2',1))
      ,('b',('2',2))
      ,('c',('2',3))
      ,('d',('3',1))
      ,('e',('3',2))
      ,('f',('3',3))
      ,('g',('4',1))
      ,('h',('4',2))
      ,('i',('4',3))
      ,('j',('5',1))
      ,('k',('5',2))
      ,('l',('5',3))
      ,('m',('6',1))
      ,('n',('6',2))
      ,('o',('6',3))
      ,('p',('7',1))
      ,('q',('7',2))
      ,('r',('7',3))
      ,('s',('7',4))
      ,('t',('8',1))
      ,('u',('8',2))
      ,('v',('8',3))
      ,('w',('9',1))
      ,('x',('9',2))
      ,('y',('9',3))
      ,('z',('9',4))
      ,('.',('0',1))
      ,(',',('0',2))
      ,('?',('0',3))
      ,('!',('0',4))
      ,('\n',('0',5))
      ,('`',('0',6))
      ,('"',('0',7))
      ,('0',('0',8))
      ,('-',('0',9))
      ,('(',('0',10))
      ,(')',('0',11))
      ,('@',('0',12))
      ,('/',('0',13))
      ,(':',('0',14))
      ,('_',('0',15))
      ,('1',('1',1))
      ,('2',('2',4))
      ,('3',('3',4))
      ,('4',('4',4))
      ,('5',('5',4))
      ,('6',('6',4))
      ,('7',('7',5))
      ,('8',('8',4))
      ,('9',('9',5))
      ,(' ',(' ',1))
      ,('#',('#',1))]




-- | Die Funtion gibt getypte  eingabe als Zahl zurueck
text2sms :: String -> String 
text2sms [] = []
text2sms (x:xs)
    | length (x:xs) > 1 = helper x ++" "++ text2sms xs
    | length (x:xs) == 1 = helper x

-- | Diese Funktion eine hilf Funktion fuer text2sms
helper :: Char -> String
helper x
    | x == 'a' = "2"
    | x == 'b' = "22"
    | x == 'c' = "222"
    | x == '2' = "2222"
    | x == 'd' = "3"
    | x == 'e' = "33"
    | x == 'f' = "333"
    | x == '3' = "3333"
    | x == 'g' = "4"
    | x == 'h' = "44"
    | x == 'i' = "444"
    | x == '4' = "4444"
    | x == 'j' = "5"
    | x == 'k' = "55"
    | x == 'l' = "555"
    | x == '5' = "5555"
    | x == 'm' = "6"
    | x == 'n' = "66"
    | x == 'o' = "666"
    | x == '6' = "6666"
    | x == 'p' = "7"
    | x == 'q' = "77"
    | x == 'r' = "777"
    | x == 's' = "7777"
    | x == '7' = "77777"
    | x == 't' = "8"
    | x == 'u' = "88"
    | x == 'v' = "888"
    | x == '8' = "8888"
    | x == 'w' = "9"
    | x == 'x' = "99"
    | x == 'y' = "999"
    | x == 'z' = "9999"
    | x == '9' = "99999"
    | x == '*' = "*"
    | x == '#' = "#"
    | x == '.' = "0"
    | x == ',' = "00"
    | x == '?' = "000"
    | x == '!' = "0000"
    | x == '`' = "00000"
    | x == '"' = "000000"
    | x == '0' = "0000000"
    | x == '-' = "00000000"
    | x == '(' = "000000000"
    | x == ')' = "0000000000"
    | x == '0' = "00000000000"
    | x == '@' = "000000000000"
    | x == '/' = "0000000000000"
    | x == ':' = "00000000000000"
    | x == '_' = "000000000000000"
    




sms2text :: String -> String
sms2text x = helper3 (helper2 x)


helper2 :: String -> [String]
helper2 x = words x


helper3 :: [String] -> String
helper3 [] = []
helper3 (x:xs)
    | head x == '1' && length x == 1 = "\n" ++ helper3 xs
    | head x == '2' && length x == 1 = "a" ++ helper3 xs
    | head x == '2' && length x == 2 = "b" ++ helper3 xs
    | head x == '2' && length x == 3 = "c" ++ helper3 xs
    | head x == '2' && length x == 4 = "2" ++ helper3 xs
    | head x == '3' && length x == 1 = "d" ++ helper3 xs
    | head x == '3' && length x == 2 = "e" ++ helper3 xs
    | head x == '3' && length x == 3 = "f" ++ helper3 xs
    | head x == '3' && length x == 4 = "3" ++ helper3 xs
    | head x == '4' && length x == 1 = "g" ++ helper3 xs
    | head x == '4' && length x == 2 = "h" ++ helper3 xs
    | head x == '4' && length x == 3 = "i" ++ helper3 xs
    | head x == '4' && length x == 4 = "4" ++ helper3 xs
    | head x == '5' && length x == 1 = "j" ++ helper3 xs
    | head x == '5' && length x == 2 = "k" ++ helper3 xs
    | head x == '5' && length x == 3 = "l" ++ helper3 xs
    | head x == '5' && length x == 4 = "5" ++ helper3 xs
    | head x == '6' && length x == 1 = "m" ++ helper3 xs
    | head x == '6' && length x == 2 = "n" ++ helper3 xs
    | head x == '6' && length x == 3 = "o" ++ helper3 xs
    | head x == '6' && length x == 4 = "6" ++ helper3 xs
    | head x == '7' && length x == 1 = "p" ++ helper3 xs
    | head x == '7' && length x == 2 = "q" ++ helper3 xs
    | head x == '7' && length x == 3 = "r" ++ helper3 xs
    | head x == '7' && length x == 4 = "s" ++ helper3 xs
    | head x == '7' && length x == 5 = "7" ++ helper3 xs
    | head x == '8' && length x == 1 = "t" ++ helper3 xs
    | head x == '8' && length x == 2 = "u" ++ helper3 xs
    | head x == '8' && length x == 3 = "v" ++ helper3 xs
    | head x == '8' && length x == 4 = "8" ++ helper3 xs
    | head x == '9' && length x == 1 = "w" ++ helper3 xs
    | head x == '9' && length x == 1 = "x" ++ helper3 xs
    | head x == '9' && length x == 2 = "y" ++ helper3 xs
    | head x == '9' && length x == 3 = "z" ++ helper3 xs
    | head x == '9' && length x == 4 = "9" ++ helper3 xs
    | head x == '*' && length x == 1 = "*" ++ helper3 xs
    | head x == '#' && length x == 1 = "#" ++ helper3 xs
    | head x == '0' && length x == 1 = "." ++ helper3 xs
    | head x == '0' && length x == 2 = "," ++ helper3 xs
    | head x == '0' && length x == 3 = "?" ++ helper3 xs
    | head x == '0' && length x == 4 = "!" ++ helper3 xs
    | head x == '0' && length x == 5 = "`" ++ helper3 xs
    | head x == '0' && length x == 7 = "``" ++ helper3 xs
    | head x == '0' && length x == 8 = "0" ++ helper3 xs
    | head x == '0' && length x == 9 = "-" ++ helper3 xs
    | head x == '0' && length x == 10 = "(" ++ helper3 xs
    | head x == '0' && length x == 11 = ")" ++ helper3 xs
    | head x == '0' && length x == 12 = "@" ++ helper3 xs
    | head x == '0' && length x == 13 = "/" ++ helper3 xs
    | head x == '0' && length x == 14 = ":" ++ helper3 xs
    | head x == '0' && length x == 15 = "_" ++ helper3 xs


