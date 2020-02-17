module ElimEvensDoubleOdds where


-- | using list comprehension
elimEvensDoubleOdds :: [Int] -> [Int]
elimEvensDoubleOdds ls = [x * 2 | x <-ls, x `mod` 2 /=0] 

-- | eliminate even number and multiply each one using lambda expression 
elim2 ls = map (*2)(filter (\x -> odd x ) ls)



