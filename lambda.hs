
-- |add x y = x + y
add = \x -> (\y -> x + y) -- | lambda version

const' :: a -> b -> a
-- | const' x _ = x
const' x = \_ -> x -- | lambda version

length' :: [a] -> Int
length' xs = sum (map (const' 1) xs)

-- | return the first n odd numbers
odds :: Int -> [Int]
odds n = map (\x -> x*2 + 1) [0..n-1]

head = \(x:_) -> x -- | lambda version of head

startsWithA :: [Char] -> Bool
startsWithA ('a':_) = True
startsWithA _ = False

startsWithAB :: [Char] -> Bool
startsWithAB ('a':'b':_) = True
startsWithAB _ = False

-- | sum the first 2 elements in a list
sumTwo :: Num a => [a] -> a
sumTwo (x:y:_) = x + y

-- | return the second element in a list
second :: [a] -> a
second (_:a:_) = a

-- | examples of list generation
set = [x | x <- [1..5], x `mod` 2 == 0]
tuples = [(x,y) | x <- [1..3], y <- [4,5]]

-- | creates a relation of all pairs (x,y) such that x <= y for some x, y <= n
relation :: Int -> [(Int,Int)]
relation n = [(x,y) | x <- [1..n], y <- [x..n]]

-- | produce a list of factors of n
factors :: Int -> [Int]
factors n = [x | x <- [1..n], n `mod` x == 0]

-- | determine if a number is prime
prime :: Int -> Bool
prime n = factors n == [1,n]

-- | gives a list of the prime factors of n
primes :: Int -> [Int]
primes n = [x | x <- [2..n], prime x]

-- | combines 2 lists together creating a list of tuples
zip' :: [a] -> [b] -> [(a,b)]
zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : zip' xs ys

-- | gives a list of all adjacent pairs in a list
pairs :: [a] -> [(a,a)]
pairs xs = zip xs (tail xs)

-- | determines if a list is sorted
sorted :: Ord a => [a] -> Bool
sorted xs = and [x <= y | (x,y) <- pairs xs]

-- | determines the list of all the positions of a certain value in a list
positions :: Eq a => a -> [a] -> [Int]
positions x xs = [i | (x', i) <- zip xs [0..], x == x']



