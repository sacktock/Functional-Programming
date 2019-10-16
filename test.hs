-- | first haskell script
double x = x + x

quadruple x = double (double x)

factorial n = product [1..n]

average ns = sum ns `div` length ns

n = a `div` length xs
 where
  a = 10
  xs = [1,2,3,4,5]

last xs = xs !! (length xs - 1)

last1 xs = sum (drop (length xs - 1) xs)

init xs = take (length xs - 1) xs

init1 xs = reverse (tail (reverse xs))

-- | rotate the list 
shuffle [] = []
shuffle xs = (tail xs) ++ (take 1 xs)

-- | bubbble sort
f [] = []
f (x:xs) = f ys ++ [x] ++ f zs
 where
  ys = [a | a <- xs, a <= x]
  zs = [b | b <- xs, b > x]