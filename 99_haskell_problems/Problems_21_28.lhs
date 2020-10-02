Lists, again

\begin{code}
module Problems_21_28 where

import System.Random
import Data.List
\end{code}


21. Insert an element at a given position into a list.
Example:
$ insertAt 'X' "abcd" 2
$ "aXbcd"

\begin{code}
insertAt :: a -> [a] -> Int -> [a]
insertAt y xs 1 = y:xs
insertAt y (x:xs) k = x:(insertAt y xs (k-1))
\end{code}


22. Create a list containing all integers within a given range.
Example:
$ range 4 9
$ [4,5,6,7,8,9]

\begin{code}
range :: Int -> Int -> [Int]
range a b
    | a == b = [a]
    | a > b = []
    | otherwise = a:(range (a+1) b)
\end{code}


23. Extract a given number of randomly selected elements from a list.
Example:
$ rnd_select "abcdefgh" 3 >>= putStrLn
$ eda

\begin{code}
rnd_select :: [a] -> Int -> IO [a]
rnd_select xs n = do
    gen <- getStdGen
    return $ take n [ xs !! k | k <- randomRs (0, (length xs)-1) gen ]
\end{code}

We have to do it in the `do` block since we are using IO-based random generator.
Had to import `Standard.Random` to work with it. We get the generator, then
generate list of indices. We select elements at those indices and then take first
`n` of them.


24. Lotto: Draw N different random numbers from the set 1..M.
Example:
$ diff_select 6 49
$ [23,1,17,33,21,37]

First attempt wouldn't produce distinct results.

\begin{code}
diff_select_bad :: Int -> Int -> IO [Int]
diff_select_bad n m= do
    gen <- getStdGen
    return $ take n (randomRs (1, m) gen)
\end{code}

Ideally, we would take an item, remove it from the set, and then draw next.

\begin{code}
remove :: (Eq a) => a -> [a] -> [a]
remove y xs = [x | x <- xs, x /= y]
\end{code}

Single draw would be simple.

\begin{code}
select :: Int -> IO Int
select m = do
    gen <- getStdGen
    let (k, nextGen) = randomR (1, m) gen
    return k
\end{code}

Now just need to do the same but in recursion with removing items.
Will use a helper function that will take items from lists and use a generator.

\begin{code}
diff_select :: Int -> Int -> IO [Int]
diff_select n m = do
    gen <- getStdGen
    return (list_diff_select n [1..m] gen)

list_diff_select :: (Eq a, RandomGen g) => Int -> [a] -> g -> [a]
list_diff_select 0 _ _ = []
list_diff_select n xs gen = x:(list_diff_select (n-1) (remove x xs) next_gen)
    where (k, next_gen) = randomR (0, (length xs) - 1) gen
          x = xs !! k
\end{code}


25. Generate a random permutation of the elements of a list.
Example:
$ rnd_permu "abcdef"
$ "badcef"

Easy, given our `list_diff_select` function.

\begin{code}
rnd_permu :: (Eq a) => [a] -> IO [a]
rnd_permu xs = do
    gen <- getStdGen
    return $ list_diff_select (length xs) xs gen
\end{code}


Problem 26
(**) Generate the combinations of K distinct objects chosen from the N
elements of a list.

In how many ways can a committee of 3 be chosen from a group of 12 people?
We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the
well-known binomial coefficients). For pure mathematicians, this result may
be great. But we want to really generate all the possibilities in a list.

$ combinations 3 "abcdef"
$ ["abc","abd","abe",...]

How to go about this? First idea - combinations of length 1, then append.

\begin{code}
combinations :: (Eq a) => Int -> [a] -> [[a]]
combinations 1 xs = [[x] | x <- nub xs]
combinations n xs = [y:ys
                     | (y:[]) <- combinations 1 xs
                     , ys <- combinations (n-1) xs
                     , (length $ nub $ y:ys) == n]
\end{code}


27. Group the elements of a set into disjoint subsets.

a) In how many ways can a group of 9 people work in 3 disjoint subgroups
of 2, 3 and 4 persons? Write a function that generates all the possibilities
and returns them in a list.

b) Generalize the above predicate in a way that we can specify a list of group
sizes and the predicate will return a list of groups.

Note that we do not want permutations of the group members; i.e.
((ALDO BEAT) ...) is the same solution as ((BEAT ALDO) ...).
However, we make a difference between ((ALDO BEAT) (CARLA DAVID) ...) and
((CARLA DAVID) (ALDO BEAT) ...).

You may find more about this combinatorial problem in a good book on
discrete mathematics under the term "multinomial coefficients".

Example in Haskell:

$ group [2,3,4] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
$ [[["aldo","beat"],["carla","david","evi"],["flip","gary","hugo","ida"]],...]
$ (altogether 1260 solutions)

$ group [2,2,5] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
$ [[["aldo","beat"],["carla","david"],["evi","flip","gary","hugo","ida"]],...]
$ (altogether 756 solutions)