-- fst
-- snd

modulus :: Num a => (a, a) -> a
-- floating
modulus (x, y) = x^2 + y^2


ackermann 0 n = n + 1
ackermann m 0 = ackermann (m - 1) 1
ackermann m n = ackermann (m - 1) (ackermann m (n - 1))


ackermann' m n = case (m, n) of (0, n) -> n + 1
                                (m, 0) -> ackermann (m - 1) 1
                                (m, n) -> ackermann (m - 1) (ackermann m (n - 1))
