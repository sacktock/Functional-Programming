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

-- Simple binary tree manipulation
data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Eq, Show)

makeBalancedTree' :: Int -> [Int] -> (Tree Int, [Int])
makeBalancedTree' 0 _ = (undefined, [])
makeBalancedTree' 1 (i:is) = (Leaf i, is)
makeBalancedTree' n is = (Node l r, is'')
  where (l, is') = makeBalancedTree' n' is
        m' = n `div` 2
        n' = m' + (n `rem` 2)
        (r, is'') = makeBalancedTree' m' is'

makeBalancedTree :: Int -> Tree Int
makeBalancedTree n = fst $ makeBalancedTree' n [1..]

mirrorTree :: Tree a -> Tree a
mirrorTree l@(Leaf _) = l
mirrorTree (Node l r) = Node (mirrorTree r) (mirrorTree l)

numLeaves :: Tree a -> Int
numLeaves = undefined

balanced :: Tree a -> Bool
balanced = undefined

toTree :: [a] -> Tree a
toTree = undefined

mirror :: Tree a -> Tree a -> Bool
mirror = undefined

symmetric :: Tree a -> Bool
symmetric = undefined

toList :: Tree a -> [a]
toList = undefined

-- Representing simple algebraic expressions
data Expr = Val Int | Add Expr Expr
  deriving (Eq, Show)

eval :: Expr -> Int
eval = undefined

collect :: Expr -> [Expr]
collect = undefined

size :: Expr -> Int
size = undefined

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde = undefined

-- Now using folde
eval' :: Expr -> Int
eval' = folde id (+)

collect' :: Expr -> [Expr]
collect' = undefined

size' :: Expr -> Int
size' = undefined

