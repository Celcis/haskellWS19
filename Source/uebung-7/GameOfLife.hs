{-# LANGUAGE ParallelListComp #-}

module GameOfLife where

-- Repraesentiert den Zustand einer Zelle.
-- D steht fuer dead, also tot, A steht fuer alive, also lebend,
-- W steht fuer wall (ein Wandelemen). 
-- Wandelemente wurden hinzugefuegt, um laestige Randfaelle 
-- zu vermeiden. Ein Wandelement verhaelt sich zum Zustandsuebergang neutral.
data Cell = D | A | W
            deriving Eq

instance Show Cell where
  show dead = "_"


newtype Grid = Grid { grid :: [[Cell]] }


instance Show Grid where
  show = undefined


-- Die Regeln, nach denen der neue Zustand einer Zelle berechnet wird  
rules :: Cell -> [Cell] -> Cell
rules = undefined


-- Repraesentiert eine Umgebung (engl. neighbourhood),
-- in der neun Zellen vorhanden sind. Dieser Datentyp
-- repraesentiert genau die Umgebungen, welche fuer das 
-- Game of Life benoetigt werden.      
type Neighbourhood = (Cell, Cell, Cell,
                      Cell, Cell, Cell,
                      Cell, Cell, Cell)


-- Bildet ein Grid auf eine Liste von Listen von Umgebungen ab.
-- Jede innere Liste aus dem Grid wird auf eine Liste von Umgebungen
-- abgebildet:
--
-- Grid [ [W, W,  W,  W]
--      , [W, e1, e2, W]
--      , [W  e3, e4, W] 
--      , [W, W,  W,  W] ]
--             ~~>
--  [ [ (W, W,  W        (W,  W,  W,
--       W, e1, e2,  ,    e1, e2, W,
--       W, e3, e4)       e3, e4, W) ]
--  
--  , [ (W, e1, e2,      (e1, e2, W
--       W, e3, e4,  ,    e3, e4, W
--       W, W,  W )       W,  W,  W) ] ]
--
-- Hier wird klar, weshalb der Zustand W für eine Zelle benoetigt wird.
-- Da zu den aeußeren Elementen auf dieser Weise keine Umgebung 
-- gebildet werden kann, werden Dummy-Elemente benoetig, die bei der 
-- Bildung der Umgebung wegfallen koennen. Daher wurde zu den
-- Zellzustaenden D und A noch ein dritter (W) hinzugefuegt.
-- Eine passende Umgebung zu einer Zele meint, dass diese Zelle in 
-- der Mitte einer Umgebung ist:
--
--       (*, *, *,   U ist eine passende Umgebung zur Zelle z.
--  U :=  *, z, *,    
--        *, *, *)
-- 
-- Diese Funktion verwendet die Funktionen threeLines und blocks.
neighbourhoods :: Grid -> [[Neighbourhood]]
neighbourhoods g = undefined
                      
                                    
-- Berechnet ein neues Grid aus dem Zustand eines bestehenden Grids.
-- Dafuer werden mit der Funktion neighbourhoods die benoetigten 
-- Umgebungen berechnet. Auf diesen Umgebungen werden dann die Regeln 
-- angwendet.
nextGrid :: Grid -> Grid
nextGrid = Grid . wrapWallsAround 
                . undefined

            
-- Bildet eine Liste auf eine Liste von Dreier-Tupeln ab,
-- wobei es fuer alle drei aufeianderfolgenden Listenelemente 
-- ein Tupel gibt:
-- [1..5] ~~> [(1,2,3),(2,3,4),(3,4,5)]
threeLines :: [a] -> [(a,a,a)] 
threeLines l = undefined


-- Bildet drei Listen auf eine Liste von Neuner-Tupeln ab.
-- Dabei sind die ersten drei Elemente eines Tupels drei 
-- aufeinanderfolgende Listenelemente der ersten Liste,
-- die vierten, die fünften, und die sechsten Elemente
-- eines Tupels sind drei aufeinanderfolgende Elemente der
-- zweiten Liste und die letzten Elemente sind drei 
-- aufeinanderfolgende Elemente der dritten Listen:
-- blocks [1..4] 
--        [5..8] 
--        [9..12]

--    ~~> [(1,2,3,     (2, 3, 4,
--          5,6,7,   ,  6, 7, 8,
--          9,10,11)     10,11,12)]
blocks :: [a] -> [b] -> [c] -> [(a,a,a,b,b,b,c,c,c)] 
blocks l1 l2 l3 
  = undefined


count :: (Eq a) => a -> [a] -> Int
count x = length . filter ((==) x)

  
-- Zip9 gibt es nicht in Prelude, daher wurde ein zip9 mit der vom
-- GHC angebotenen Parallel-List-Comprehension-Erweiterung geschrieben.
zip9 :: [a] -> [b] -> [c] -> 
        [d] -> [e] -> [f] -> 
        [g] -> [h] -> [i] -> 
        [(a,b,c,d,e,f,g,h,i)]
zip9 l1 l2 l3 l4 l5 l6 l7 l8 l9
  = [(x1, x2, x3, 
      x4, x5, x6, 
      x7, x8, x9) | x1 <- l1 | x2 <- l2 | x3 <- l3 
                  | x4 <- l4 | x5 <- l5 | x6 <- l6
                  | x7 <- l7 | x8 <- l8 | x9 <- l9
                  ]
              
                  
-- Nimmt eine List von Listen von Zellen und gibt, falls 
-- die inneren Listen nicht gleich lang sind, Nothing zurueck.
-- Andernfalls wird der uebergebene Wert in einen Wert von Grid 
-- gepackt und als Just zurueckgegeben. Außerdem werden noch Zellen
-- vom Zustand W (wall, Wand) um diese Listen gezogen, siehe 
-- Funktion wrapWallsAround.
newGrid :: [[Cell]] -> Maybe Grid
newGrid cells = if valid cells
                then Just $ Grid $ wrapWallsAround cells
                else Nothing
               where
                 valid []    = True
                 valid cells = all (not . null) cells 
                            && all (\ l -> length l == (length . head) cells) cells 
                        

-- Zieht um eine Liste von Zellen Zellen vom Zustand W (Wand),
-- z.B.:                  [ [W, W, W, W]
--      [ [ *, * ]        , [W, *, *, W]
--      , [ *, * ] ] ~~>  , [W, *, *, W]
--                        , [W, W, W, W] ] 
-- * steht für eine Zelle mit beliebigem Zustand.
wrapWallsAround :: [[Cell]] -> [[Cell]]
wrapWallsAround []    = [ [W, W], [W, W] ]
wrapWallsAround cells = let walls  = replicate ((length $ head cells) + 2) W 
                            cells' = map ( \ list -> W : list ++ [W] ) cells
                        in walls : cells' ++ [walls] 
              
                        
-- Wendet die Regeln (rules) auf eine Umgebung (Neighbourhood) an
applyRules :: Neighbourhood -> Cell
applyRules (lu, u, ru,
            l,  m, r,
            ld, d, rd) = rules m [lu, u, ru, l, r, ld, d, rd]
