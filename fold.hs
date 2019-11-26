length' :: [a] -> Int
length' = foldr (\_ n -> 1 + n) 0

and' :: [Bool] -> Bool
and' xs = foldr (&&) True xs 

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (f) [] xs