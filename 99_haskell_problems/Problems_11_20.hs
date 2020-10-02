module Problems_11_20 where
import Problems_1_10
-- Lists, continued


-- 11. (*) Modified run-length encoding. Modify the result of problem 10 in
--     such a way that if an element has no duplicates it is simply copied into
--     the result list. Only elements with duplicates are transferred as (N E)
--     lists.
data MaybeMultiple a = Single a | Multiple Int a
    deriving (Show)

encodeModified :: (Eq a) => [a] -> [MaybeMultiple a]
encodeModified xs = [countOccurences y | y <- pack xs]
    where countOccurences (x:[]) = Single x
          countOccurences xs = Multiple (length xs) (head xs)

encodeModified' :: (Eq a) => [a] -> [MaybeMultiple a]
encodeModified' = map encodeHelper . encode
    where encodeHelper :: (Eq a) => (Int, a) -> MaybeMultiple a
          encodeHelper (1, x) = Single x
          encodeHelper (n, x) = Multiple n x


-- 12. (**) Decode a run-length encoded list. Given a run-length code list
--     generated as specified in problem 11. Construct its uncompressed
--     version.
decodeModified :: [MaybeMultiple a] -> [a]
decodeModified [] = []
decodeModified (Single x:xs) = x:decodeModified xs
decodeModified (Multiple n x:xs) = (take n $ repeat x) ++ decodeModified xs


-- 13. (**) Run-length encoding of a list (direct solution).
--     Implement the so-called run-length encoding data compression method
--     directly. I.e. don't explicitly create the sublists containing the
--     duplicates, as in problem 9, but only count them. As in problem P11,
--     simplify the result list by replacing the singleton lists (1 X) by X.
encodeDirect :: (Eq a) => [a] -> [MaybeMultiple a]
encodeDirect [] = []
encodeDirect (x:xs) = map encodeConverter (encodeDirectHelper xs (1, x) [])
    where encodeConverter :: (Eq a) => (Int, a) -> MaybeMultiple a
          encodeConverter (1, x) = Single x
          encodeConverter (n, x) = Multiple n x

encodeDirectHelper :: (Eq a) => [a] -> (Int, a) -> [(Int, a)] -> [(Int, a)]
encodeDirectHelper [] e es = es ++ [e]
encodeDirectHelper (x:xs) e@(n, y) es
    | x == y    = encodeDirectHelper xs (n + 1, y) es
    | otherwise = encodeDirectHelper xs (1, x) (es ++ [e])


-- 14. (*) Duplicate the elements of a list.
dupli :: [a] -> [a]
dupli = foldr (\x acc -> x:x:acc) []


-- 15. (**) Replicate the elements of a list a given number of times.
repli :: [a] -> Int -> [a]
repli xs n = foldr (\x acc -> (take n $ repeat x) ++ acc) [] xs


-- 16. (**) Drop every N'th element from a list.
dropEvery :: [a] -> Int -> [a]
dropEvery xs n = [x | (i, x) <- zip [1..] xs, i `mod` n /= 0]


-- 17. (*) Split a list into two parts; the length of the first part is given.
--     Do not use any predefined predicates.
split :: [a] -> Int -> ([a], [a])
split xs 0 = ([], xs)
split [] _ = error "Split out of bounds."
split (x:xs) n = let (nextSplit1, nextSplit2) = split xs (n-1)
                 in (x:nextSplit1, nextSplit2)


-- 18. (**) Extract a slice from a list. Given two indices, i and k, the slice
--     is the list containing the elements between the i'th and k'th element of
--     the original list (both limits included). Start counting the elements
--     with 1.
slice :: [a] -> Int -> Int -> [a]
slice xs ns nf = startCut
    where (finishCut, _) = split xs nf
          (_, startCut) = split finishCut (ns-1)


-- 19. (**) Rotate a list N places to the left.
--     Hint: Use the predefined functions length and (++).
rotate :: [a] -> Int -> [a]
rotate xs r
    | r > 0 = h2 ++ h1
    | r < 0 = rh2 ++ rh1
    | otherwise = xs
    where
        (h1, h2) = split xs r
        (rh1, rh2) = split xs (length xs + r)


-- 20. (*) Remove the K'th element from a list.
removeAt :: Int -> [a] -> [a]
removeAt 1 (x:xs) = xs
removeAt k (x:xs) = x:(removeAt (k-1) xs)