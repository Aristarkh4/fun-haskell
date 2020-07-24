-- Quicksort. Selecting first element as a pivot.
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = smallSorted ++ [x] ++ bigSorted
    where smallSorted = quicksort [a | a <- xs, a < x]
          bigSorted = quicksort [a | a <- xs, a >= x]