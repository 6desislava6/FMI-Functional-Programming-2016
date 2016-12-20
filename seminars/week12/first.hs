zipWith' f [] _ = []
zipWith' f _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' f = (\x y -> flip y x)

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' p (x: xs)
  | p x = x : takeWhile' p xs
  | otherwise = []

dropWhile' _ [] = []
dropWhile' p (x: xs)
  | p x = dropWhile' p xs
  | otherwise = x:xs

compress [] = []
compress (x:xs) = (x, length(takeWhile' (== x) (x:xs))) : compress (dropWhile' (== x) xs)


quicksort [] = []
quicksort (x:xs) = quicksort [i | i <- xs, i < x] ++ [x] ++  quicksort [i | i <- xs, i >= x]

maxRepeated [] = 0
maxRepeated l = fst . head $ filter (\(x, y) -> y == maxLength) compressed
  where
    compressed = compress l
    maxLength = maximum $ map snd compressed

makeSet [] = []
makeSet [x] = [x]
makeSet (x : xs) = x : (makeSet $ filter (/= x) xs)

histogram [] = []
histogram l = [(x, length (filter (== x) l)) | x <- makeSet l]

maxDistance l = maximum [ sqrt $ (x - u)^2 + (y - v)^2| (x, y) <- l, (u, v) <- l]

compose f n
  | n == 0 = id
  | n == 1 = f
  | otherwise = f . (compose f (n - 1))

compositions f = [compose f i| i <- [1..]]
