let numbers = [1,2,3,4,5,6,7,8,9,10]

swap (x, y) = (y, x)

pair x y = (x, y)

double x = x * x

palindrome xs = xs == reverse xs

-- | apply f to x twice
twice f x = f (f x)

-- | return the penultimate item in a list if there is one
-- | O(1) because we drop the tail and then take the tail
myButLast (xs) = last (init xs)

-- | takes the seventh item in a list if it exists
seventh (xs) = last (take 7 xs)

-- | take the first and seventh item in a list and puts it in a tuple
take17 (xs) = (head xs, seventh xs)

-- |safetail (x:xs) = xs
-- |safetail [] = []

-- |safetail xs 
-- | | null xs = []
-- | | otherwise = (tail xs)
	
safetail xs = if null xs then [] else (tail xs)





