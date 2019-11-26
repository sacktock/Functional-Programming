-- | take 10 primes gives us the first 10 primes
primes :: [Int]
primes = sieve [2..]

sieve :: [Int] -> [Int]
sieve (x:xs) = [x] ++ sieve [a | a <- xs, a `mod` x /= 0]

mult :: [[Int]] -> [Int] -> [Int]
mult (xs:xss) ys = if xss == [] then [foldl (+) 0 (zipWith (*) xs ys)] else [foldl (+) 0 (zipWith (*) xs ys)] ++ mult xss ys

pythTriple :: [Int] -> (Int, Int, Int)
pythTriple [a,b] = (a^2 - b^2, 2*a*b, a^2+b^2) 

 -- concatMap :: (a -> [b]) -> [a] -> [b]

concat' :: [[Int]] -> [[Int]]
concat' (xs:xss) = if xss == [] then [mult [[2,-1],[1,0]] xs] ++ [mult [[2,1],[1,0]] xs]  ++ [mult [[1,2],[0,1]] xs] else [mult [[2,-1],[1,0]] xs] ++ [mult [[2,1],[1,0]] xs]  ++ [mult [[1,2],[0,1]] xs] ++ concat' xss


treeGen :: [[Int]] -> [[Int]]
treeGen xss = xss ++ treeGen (concat' xss)

pyths :: [(Int,Int,Int)]
pyths = map pythTriple (treeGen [[2,1]])
