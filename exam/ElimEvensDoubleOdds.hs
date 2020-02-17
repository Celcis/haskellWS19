module ElimEvensDoubleOdds where


elimEvensDoubleOdds :: [Int] -> [Int]
elimEvensDoubleOdds ls = [x * 2 | x <-ls, x `mod` 2 /=0] 

elim2 ls = map (*2)(filter (\x -> odd x ) ls)
