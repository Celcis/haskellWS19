module Uebung_6 where

-- Der Datentyp für die Repräsentation des Stadt-Graphen mit gewichteten Kanten
data CityNet a = District a [(CityNet a, Weight)] deriving (Eq, Show)

type Weight = Int

-- Die Bezirke Bremens als Aufzählungstyp
data Bremen = Viertel  | Innenstadt  | Schwachhausen | Horn | 
              Uni      | Findorff    | Walle         | Gröpelingen | 
              Neustadt | Habenhausen | Kattenturm    | Hemelingen
              deriving (Eq, Ord, Show)

-- Der Stadt-Graph für Bremen
k01 = District Viertel [(k02, 5), (k03, 5), (k04, 7), (k05, 6), (k06, 7)]
k02 = District Innenstadt [(k07, 3), (k08, 6), (k03, 8), (k01, 5), (k09, 9)]
k03 = District Schwachhausen [(k08, 7), (k10, 4), (k04, 4), (k01, 5), (k02, 8)]
k04 = District Horn [(k10, 2), (k05, 5), (k01, 7), (k03, 4)]
k05 = District Hemelingen [(k01, 6), (k04, 5), (k11, 7)]
k06 = District Habenhausen [(k01, 7), (k11, 4)]
k07 = District Walle [(k12, 4), (k08, 5), (k02, 3)]
k08 = District Findorff [(k07, 5), (k10, 6), (k03, 7), (k02, 6)]
k09 = District Neustadt [(k02, 9), (k11, 5)]
k10 = District Uni [(k12, 3), (k04, 2), (k03, 4), (k08, 6)]
k11 = District Kattenturm [(k09, 5), (k06, 4), (k05, 7)]
k12 = District Gröpelingen [(k10, 3), (k07, 4)]

type Path a = [a]

type PathWithCosts a = (Path a, Weight)

data Tour a = Tour (Path a) Weight (CityNet a) deriving (Eq, Show)


-- ====================================================


-- ===================================================
-- |1. Die Funktion gibt eingegene Tour als Pfad zurueck 
pathOfTour :: Tour a -> Path a
pathOfTour (Tour [x,y] n ntz) = [x,y] 

-- |2. Die Funktion gibt kosten vom Tor zurueck
costOfTour :: Tour a -> Int
costOfTour (Tour [x,y] n ntz) = n 

-- |3. Die Funktion gibt die Netz  zurueck
netOfTour :: Tour a -> CityNet a
netOfTour (Tour [x,y] n ntz) = ntz

-- |4.Die Funktion fuegt eine neue Node hinzu
addNodeToTour :: a -> Tour a -> Tour a
addNodeToTour newT (Tour [x,y] n ntz) =  (Tour [newT] n ntz)

-- |5. Die Funktion prueft, ob gegebene Node im Tour ist 
nodeInTour :: Eq a => a -> Tour a -> Bool
nodeInTour s (Tour [x,y] n ntz)
    | (Tour [x,y] n ntz) == (Tour [s] n ntz) =  True
    | otherwise = False
