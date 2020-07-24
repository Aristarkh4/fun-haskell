doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y
doubleSmallNumber x = if x < 100
                        then doubleMe x
                        else x


myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]


-- which right triangle that has integers for all sides and all sides
-- equal to or smaller than 10 has a perimeter of 24?
answerTriangles = [(a, b, c)
                    | c <- [1..10], b <- [1..c], a <- [1..b],
                    a + b + c == 24,
                    c^2 == a^2 + b^2]
