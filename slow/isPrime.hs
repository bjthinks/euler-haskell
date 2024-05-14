fIsPrime :: (Integral a) => a -> Bool
-- If n < 4,759,123,141 is a 2, 7 and 61-SPRP, then n is prime.
fIsPrime n
  | or (map (divides n) (take 25 primes)) = True
  | n < 4759123141 = isSPRP n 2 && isSPRP n 7 && isSPRP n 61
  | otherwise = isPrime n

isSPRP :: (Integral a) => a -> a -> Bool
isSPRP b n = undefined
  where
    -- n-1 = 2^s d
    s = twos (n-1)
    d = div (n-1) (2^s)
    twos k
      | mod k 2 == 0 = 1 + twos (div k 2)
      | otherwise = 0

powmod :: (Integral a, Integral b) => a -> b -> a -> a
powmod a n m
  | m <= 1 = error "illegal modulus in powmod"
  | n == 0 = 1
  | n == 1 = mod a m
  | mod n 2 == 0 = powmod (mod (a^2) m) (div n 2) m
  | otherwise = mod (a * powmod a (n-1) m) m
