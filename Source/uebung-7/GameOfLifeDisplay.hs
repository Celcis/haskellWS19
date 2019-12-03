module GameOfLifeDisplay where

-- Dieses Modul realisiert eine Ausgabe auf der Konsole.
-- Dafuer muss die Funktion run ausgefuehrt werden. 
-- Bei Unix kann die Ausgabe mit Strg + C abgebrochen werden.
-- Für dieses Modul sollte die ANSI-Terminal-Bibliothek installiert 
-- werden (cabal install ansi-terminal). Diese stellt die Funktion 
-- clearScreen bereit und funktioniert auch auf Windows.


import GameOfLife 
import qualified Grids as G

import Text.Read(readMaybe) 
import Control.Concurrent(threadDelay)
import System.Console.ANSI(clearScreen)

run = do
  putStrLn $ "Gib mir eine Nummer zwischen 0 und " ++ (show $ length G.grids - 1) ++ "."
  i <- getIndex
  let maybeGrid = giveMaybeGridByIndex i
  case maybeGrid of
    Just g -> displayGameOfLife g
    _      -> putStrLn $ "Fehler in der Grids-Datei!\n"
                ++ "Pruefe, ob es sich bei deinem gewaehltem Grid um ein gueltiges handelt"
             
getIndex :: IO Int
getIndex = do
  s <- getLine 
  let i = (readMaybe s :: Maybe Int)
  case i of 
    Just j -> return j
    _      -> do 
        putStrLn "Etwas ist schief gelaufen, versuch es nochmal!"
        getIndex
                       
  
giveMaybeGridByIndex :: Int -> Maybe Grid
giveMaybeGridByIndex i | i < 0 || i >= length G.grids = Nothing
                       | otherwise                    = newGrid $ G.grids !! i
              
displayGameOfLife :: Grid -> IO ()
displayGameOfLife g = do 
                    putStrLn $ show g
                    threadDelay 250000
                    clearScreen
                    displayGameOfLife $ nextGrid g 
            
          
