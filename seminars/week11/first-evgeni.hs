{-head, tail, init, last-}
-- x:xs

last' xs = if length xs == 1
          then head xs
          else last (tail xs)

--

last2 :: [a] -> a
last2 [x] = x
last2 (_:xs) = (last2 xs)

head' (x:xs) = x
tail' (_:xs) = xs

init' [x] = []
init' (x:xs) = x:init'(xs)

fact 0 = 1
fact n = n * (fact (n - 1))

increasing :: (Ord a) => [a] -> Bool
increasing [] = True
increasing [x] = True
increasing (x:y:xs) = (x <= y) && increasing (y:xs)

-- otherwise == True!

abs' x
  | x == 0 = x
  | x <= 0 = -x
  | otherwise = x

-- list comprehension
-- filters, with a coma!
-- [x * x | x <- [1..10], even x, x > 3]

onlyeven xs = [x | x <- xs, even x]
-- filter even [1,2,3,4,5,6]
-- [(x, y) | x <- [1,2,3], y <- [4,5,6]]
-- tuple

-- всички двойки взаимно прости числа, които са по-малки от 100

--[(x, y) | x <- [1..100], y <- [1..100], (gcd x y) == 1]

--gcd' a b = if a == b then a

ones = 1 : ones
-- take 10 ones
-- map:: (a -> b) -> [a] -> [b]

-- zip:: [a] -> [b] -> [(a, b)]
-- zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]

nats = 0:(zipWith (+) ones nats)
-- главата + единици = опашката!

fibs = 0:1:zipWith (+) fibs (tail fibs)

takeWhile' p (x:xs) = if (p x) then x:takeWhile' p xs else []
-- Защото функциите са частични:
-- takeWhile (<1000) fibs
-- takeWhile (1000>) fibs


-- частично приложена функция
-- (((<) 3) 4) -> (<) 3 _
-- (< 3) -> _ < 3
-- когато е инфиксна, но
-- опакована в скобки - държи като нормална функция
-- ако не - допълва се празното!!!

-- (f.g) 3 = 7
-- f.g.f.g $ 10
-- $, infix ляво-асоциативна
-- fixity
-- тя има нисък приоритет - най-нисък от всички

-- pow f n
-- f 0 - id
-- f n + 1 = f . pow f n
fpow :: (a -> a) -> Int -> (a -> a)





