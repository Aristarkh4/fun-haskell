-- 1. (*) Find last element of the list
myLast :: [a] -> a
myLast [] = error "The empty list does not have a last element."
myLast (x:[]) = x
myLast (x:xs) = myLast xs


-- 2. (*) Find the last but one element of a list.
myButLast :: [a] -> a
myButLast [] = error "The empty list does not have last but one element."
myButLast (x:[]) = error "Singleton list does not have last but one element."
myButLast (x:y:[]) = x
myButLast (x:y:xs) = myButLast (y:xs)


-- 3. (*) Find the K'th element of a list. The first element in the list is
--    number 1.
element_at :: (Integral a) => [b] -> a -> b
element_at [] _ = error "Required position does not exist in the list."
element_at (x:xs) p
    | p < 1     = error "Position in the list should be >= 1."
    | p == 1    = x
    | otherwise = element_at xs (p-1)


-- 4. (*) Find the number of elements of a list.
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs


-- 5. (*) Reverse a list.
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]


-- 6. (*) Find out whether a list is a palindrome. A palindrome can be read
--    forward or backward; e.g. (x a m a x).
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == myReverse xs


-- 7. (**) Flatten a nested list structure. Transform a list, possibly holding
--    lists as elements into a `flat' list by replacing each list with its
--    elements (recursively).
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List []) = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)


-- 8. (**) Eliminate consecutive duplicates of list elements.
compress :: (Eq a) => [a] -> [a]
compress [] = []
compress (x:xs) = compress_helper xs x
    where compress_helper :: (Eq a) => [a] -> a -> [a]
          compress_helper [] x = [x]
          compress_helper (y:ys) x
            | x == y    = compress_helper ys y
            | otherwise = x:(compress_helper ys y)


-- 9. (**) Pack consecutive duplicates of list elements into sublists. If a
--    list contains repeated elements they should be placed in separate
--    sublists.
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = pack_helper xs [x]
    where pack_helper :: (Eq a) => [a] -> [a] -> [[a]]
          pack_helper [] ys = [ys]
          pack_helper (x:xs) (y:ys)
            | x == y    = pack_helper xs (y:y:ys)
            | otherwise = [y:ys] ++ pack_helper xs [x]


-- 10. (*) Run-length encoding of a list. Use the result of problem P09 to
--     implement the so-called run-length encoding data compression method.
--     Consecutive duplicates of elements are encoded as lists (N E) where N is
--     the number of duplicates of the element E.
encode :: (Eq a) => [a] -> [(Int, a)]
encode [] = []
encode xs = encodePacked xsPacked
    where xsPacked = pack xs
          encodePacked [] = []
          encodePacked (x:xs) = (length x, head x):(encodePacked xs)

-- Solution using list comprehensions
encode' :: (Eq a) => [a] -> [(Int, a)]
encode' xs  = [(length y, head y) | y <- pack xs]
