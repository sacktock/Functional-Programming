numbers = [1, 3, 1, 1, 2, 5]

-- | removes duplicate elements from a list
compress :: Eq a => [a] -> [a]
compress = foldl (\xs x -> if x `elem` xs then xs else xs ++[x]) []

-- | gives the pythagorian triples for any integers < x
pyths :: Int -> [(Int,Int,Int)]
pyths x = [(a, b, c) | a <- [1..x], b <- [1..x], c <- [1..x], a^2+ b^2 == c^2] 

-- | does the same but guves the unique pythagorian triples a is less than b
pyths2 :: Int -> [(Int,Int,Int)]
pyths2 x = [(a, b, c) | a <- [1..x], b <- [1..x], c <- [1..x], a < b, a^2+ b^2 == c^2] 

-- | scalar product (dot product) of 2 lists
scalar :: Num a => [a] -> [a] -> a
scalar a b = if length a == length b then sum (zipWith(*) a b) else error "lists don't have the same length"

-- | safe tail
tail' :: [a] -> [a]
tail' xs = if null xs then [] else (tail xs)

-- | merges 2 lists together
merge :: Ord a => [a] -> [a] -> [a]
merge a [] = a
merge [] b = b
merge a b = if head a < head b then [head a] ++merge (tail' a) b else [head b] ++merge (tail' b) a

-- | splits a list in half
halve :: [a] -> ([a], [a])
halve a = splitAt (((length a) + 1) `div` 2) a

-- | merge sort
mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort a = if length a == 1 then a else merge (mergeSort (fst (halve a))) (mergeSort (snd (halve a)))

