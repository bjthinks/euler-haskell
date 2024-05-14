module Euler where

import Data.Bits
import qualified Data.Heap as Heap
import Data.Ratio
import Data.List
import Control.Monad
import Data.Char
import Data.Array

-- Common infrastructure for solving Project Euler problems

-- General Haskell utility functions

-- Paul: can this be implemented via liftM2, with d being a type
-- encoded in the monad?
-- i.e.
-- instance Monad (FunctionsFrom t) ...
paste :: (a -> b -> c) -> (d -> a) -> (d -> b) -> d -> c
paste p f g d = p (f d) (g d)

equal :: (Eq b) => (a -> b) -> (a -> b) -> a -> Bool
equal = paste (==)

both :: (a -> Bool) -> (a -> Bool) -> a -> Bool
both = paste (&&)

{-
Don't know what to call this, because either is already defined in the Prelude
Would use it in P1
? :: (a -> Bool) -> (a -> Bool) -> a -> Bool
? = paste (||)
-}

-- Modular arithmetic

-- 3 mod 5 is "ModularGuts 5 3"
data Modular a = ModularGuts a a deriving (Eq,Show)

makeModular :: (Integral a) => a -> a -> Modular a
makeModular m a
  | m > 0 = ModularGuts m (mod a m)
  | otherwise = error "non-positive modulus"

liftPositive :: (Integral a) => Modular a -> a
liftPositive (ModularGuts _ a) = a

instance Integral a => Num (Modular a) where
  ModularGuts m a + ModularGuts n b = makeModular (gcd m n) (a+b)
  ModularGuts m a - ModularGuts n b = makeModular (gcd m n) (a-b)
  ModularGuts m a * ModularGuts n b = makeModular (gcd m n) (a*b)
  negate (ModularGuts m a) = makeModular m (-a)
  abs = id
  signum (ModularGuts m _) = makeModular m 1
  -- Warning: cheap trick, uses (gcd 0 a) == (gcd a 0) == a
  -- However, (gcd 0 0) throws an exception, which means that two
  -- "modular numbers" obtained via fromInteger can't be operated
  -- on by +, -, *, negate
  fromInteger i = ModularGuts 0 (fromInteger i)

-- Remove adjacent duplicates from a list
uniq :: (Eq a) => [a] -> [a]
uniq [] = []
uniq [x] = [x]
uniq (x:ys@(y:_))
  | x == y = uniq ys
  | otherwise = x : uniq ys

-- Find the maximum of a list according to a scoring function
maximumVia :: (Ord b) => (a -> b) -> [a] -> a
maximumVia f (x:xs) = maximumVia' (x,f x) (x:xs)
  where
    maximumVia' (maxItem,maxVal) [] = maxItem
    maximumVia' (maxItem,maxVal) (x:xs)
      | f x > maxVal = maximumVia' (x,f x) xs
      | otherwise = maximumVia' (maxItem,maxVal) xs

-- Generate all sublists with specified length
substrs :: Int -> [a] -> [[a]]
substrs k xs@(_:rest)
  | has k xs = (take k xs) : substrs k rest
  | otherwise = []
    where
      has 0 _ = True
      has _ [] = False
      has k (x:xs) = has (k-1) xs

-- Generate elements of a set in order of cost
generateInOrder :: Ord v => (i -> [i]) -> (i -> v) -> i -> [i]
generateInOrder succs cost start = run $ Heap.insert (item start)
                        (Heap.empty :: Heap.MinPrioHeap v i)
  where
    run h = top : (run . foldl (flip Heap.insert) h' .
                   map item . succs $ top)
      where
        Just ((_,top),h') = Heap.view h
    item x = (cost x,x)

-- Summations and other arithmetic formulae

{-# SPECIALIZE iSqrt :: Integer -> Integer #-}
{-# SPECIALIZE iSqrt :: Int -> Int #-}
iSqrt :: (Integral a) => a -> a
iSqrt 0 = 0
iSqrt 1 = 1
iSqrt n
  | n > 0 = fromInteger $ head $
            dropWhile (\x -> x*x > (toInteger n)) $
            iterate (\x -> (x + (toInteger n) `div` x) `div` 2)
            -- A guess at the square root of n, obtained from
            -- the floating point unit
            -- this must be an overestimate of the true value
            -- (div (toInteger n) 2)
            (if (toInteger n) > 10^308 then div (toInteger n) (10^154) else
               ceiling (sqrt (fromIntegral n :: Double) * 1.0000000000001))
  | otherwise = error "Negative square root"

sumUpTo :: (Integral a) => a -> a
sumUpTo n = div (n * (n+1)) 2

sumSquaresUpTo :: (Integral a) => a -> a
sumSquaresUpTo n = div (n * (n+1) * (2*n+1)) 6

-- Divisibility, primes, divisors

{-# SPECIALIZE divides :: Integer -> Integer -> Bool #-}
{-# SPECIALIZE divides :: Int -> Int -> Bool #-}
divides :: (Integral a) => a -> a -> Bool
divides n = (==0) . (mod n)

{-# SPECIALIZE divisible :: Integer -> Integer -> Bool #-}
{-# SPECIALIZE divisible :: Int -> Int -> Bool #-}
divisible :: (Integral a) => a -> a -> Bool
divisible = flip divides

{-# SPECIALIZE isEven :: Integer -> Bool #-}
{-# SPECIALIZE isEven :: Int -> Bool #-}
isEven :: (Bits a) => a -> Bool
isEven = (==0) . (.&.1)

{-# SPECIALIZE isOdd :: Integer -> Bool #-}
{-# SPECIALIZE isOdd :: Int -> Bool #-}
isOdd :: (Bits a) => a -> Bool
isOdd = (/=0) . (.&.1)

-- 1000 or more gives substantial speedup to P27, but 1000000 slows it down
_isPrimeTableSize :: Num a => a
_isPrimeTableSize = 1000

_isPrimeLookup :: (Integral a) => a -> Bool
_isPrimeLookup n = table ! (fromIntegral n)
  where
    table :: Array Int Bool
    table = listArray (2,_isPrimeTableSize)
            (boolPrimeList [2.._isPrimeTableSize]
             (takeWhile (<=_isPrimeTableSize) primes))
    boolPrimeList [] _ = []
    boolPrimeList (n:ns) [] = False : boolPrimeList ns []
    boolPrimeList (n:ns) pp@(p:ps)
      | n == p = True : boolPrimeList ns ps
      | otherwise = False : boolPrimeList ns pp

-- A probabilistic prime test would be faster for large numbers, but it hasn't been necessary yet.
{-# SPECIALIZE isPrime :: Integer -> Bool #-}
{-# SPECIALIZE isPrime :: Int -> Bool #-}
isPrime :: (Integral a) => a -> Bool
isPrime 2 = True
isPrime n
  | n < 2 = False
  | n <= _isPrimeTableSize = _isPrimeLookup n
  | otherwise = not $ any (divides n) $ takeWhile (<= iSqrtn) primes
{-
  | any (divides n) $ takeWhile (<= iSqrtn) smallPrimes = False
  | iSqrtn <= tdivLimit = True
  | not $ isSPSP 2 n = False
  | not $ isSPSP 3 n = False
  | n < 1373653 = True
  | not $ isSPSP 5 n = False
  | n < 25326001 = True
  | not $ isSPSP 7 n = False
  | n == 3215031751 = False
  | n < 118670087467 = True
  | otherwise = not $ any (divides n) $ takeWhile (<= iSqrtn) bigPrimes -}
    where
      iSqrtn = iSqrt n

{-# SPECIALIZE tdivLimit :: Integer #-}
{-# SPECIALIZE tdivLimit :: Int #-}
tdivLimit :: Integral a => a
tdivLimit = 8000

{-# SPECIALIZE smallPrimes :: [Integer] #-}
{-# SPECIALIZE smallPrimes :: [Int] #-}
smallPrimes :: Integral a => [a]
smallPrimes = takeWhile (<=tdivLimit) primes

{-# SPECIALIZE bigPrimes :: [Integer] #-}
{-# SPECIALIZE bigPrimes :: [Int] #-}
bigPrimes :: Integral a => [a]
bigPrimes = dropWhile (<=tdivLimit) primes

{-# SPECIALIZE isSPSP :: Integer -> Integer -> Bool #-}
{-# SPECIALIZE isSPSP :: Int -> Int -> Bool #-}
isSPSP :: Integral a => a -> a -> Bool
isSPSP a n = adn == 1 || elem (n-1) pows
  where
    pows = take s $ iterate (flip mod n . (^2)) adn
    adn = powmod a d n
    (d,s) = getds (n-1) 0
    getds x y
      | mod x 2 == 0 = getds (div x 2) (y+1)
      | otherwise = (x,y)

{-# SPECIALIZE powmod :: Integer -> Integer -> Integer -> Integer #-}
{-# SPECIALIZE powmod :: Int -> Int -> Int -> Int #-}
powmod :: (Integral a) => a -> a -> a -> a
powmod a 0 m = 1
powmod a p m
  | mod p 2 == 0 = powmod (mod (a^2) m) (div p 2) m
  | otherwise = mod (a * powmod a (p-1) m) m

-- ECM, or even Pollard rho, would be faster for large numbers, but it hasn't been necessary yet.
{-# SPECIALIZE factor :: Integer -> [Integer] #-}
{-# SPECIALIZE factor :: Int -> [Int] #-}
factor :: (Integral a) => a -> [a]
factor n
  | n >= 1 = factorUsing primes (iSqrt n) n
  | otherwise = error "Factor zero or negative number"
    where
      factorUsing dd@(d:ds) t n
        | d > t = case n > 1 of
          True -> [n]
          False -> []
        | divisible d n = d : factorUsing dd (iSqrt (div n d)) (div n d)
        | otherwise = factorUsing ds t n

{-# SPECIALIZE primeFactors :: Integer -> [Integer] #-}
{-# SPECIALIZE primeFactors :: Int -> [Int] #-}
primeFactors :: (Integral a) => a -> [a]
primeFactors = uniq . factor

radical :: (Integral a) => a -> a
radical = product . primeFactors

{-# SPECIALIZE numberOfDivisors :: Int -> Int #-}
numberOfDivisors :: (Integral a) => a -> Int
numberOfDivisors n = numberOfDivisors' $ factor n
  where
    numberOfDivisors' :: (Eq a) => [a] -> Int
    numberOfDivisors' [] = 1
    numberOfDivisors' xx@(x:_) = (length xfacs + 1) * numberOfDivisors' rest
      where
        (xfacs,rest) = span (==x) xx

{-# SPECIALIZE phi :: Integer -> Integer #-}
{-# SPECIALIZE phi :: Int -> Int #-}
phi :: (Integral a) => a -> a
phi n = foldl (\x d -> (div x d) * (d-1)) n (primeFactors n)

divisors :: (Integral a) => a -> [a]
divisors n = divisors' $ factor n
  where
    divisors' :: (Integral a) => [a] -> [a]
    divisors' [] = [1]
    divisors' xx@(x:xs) = [a*b | b <- divisors' rest, a <- xpows]
      where
        (xfacs,rest) = span (==x) xx
        xpows = [x^k | k <- [0..length xfacs]]

properDivisorSum :: (Integral a) => a -> a
properDivisorSum n = sum $ filter (/=n) $ divisors n

isAbundant :: (Integral a) => a -> Bool
isAbundant n = properDivisorSum n > n

{-# SPECIALIZE isAmicable :: Int -> Bool #-}
isAmicable :: (Integral a) => a -> Bool
isAmicable n = n == properDivisorSum (properDivisorSum n) && n /= properDivisorSum n

-- Combinatorics

{-# SPECIALIZE factorial :: Int -> Int #-}
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n-1)

choose :: (Integral a) => a -> a -> a
choose n k = div (div (factorial n) (factorial k)) (factorial (n-k))

-- Palindromes and other stuff specific to base 10 and string representations of numbers

isPalindromeBase :: (Integral a) => a -> a -> Bool
isPalindromeBase b = equal id reverse . digitsBase b

{-# SPECIALIZE isPalindrome :: Int -> Bool #-}
isPalindrome :: (Integral a) => a -> Bool
isPalindrome = equal id reverse . show

digits :: (Integral a) => a -> [Int]
digits = digitsBase 10

digitsBase :: (Integral a) => a -> a -> [Int]
digitsBase b n
  | n == 0 = [0]
  | n <  0 = error "digits of negative number"
  | otherwise = reverse $ rdigits n
  where
    rdigits 0 = []
    rdigits n = (fromInteger $ toInteger $ mod n b) : rdigits (div n b)

undigits :: (Integral a) => [Int] -> a
undigits = undigits' . reverse
  where
    undigits' :: (Integral a) => [Int] -> a
    undigits' [] = 0
    undigits' (x:xs) = fromInteger (toInteger x) + 10 * undigits' xs

-- Pythagorean triples

isPythagorean :: (Integral a) => (a,a,a) -> Bool
isPythagorean (a,b,c) = a^2 + b^2 == c^2

primitivePythagoreanTriplesWithPerimeterUpTo :: (Integral a) => a -> [(a,a,a)]
primitivePythagoreanTriplesWithPerimeterUpTo p = do
  m <- [2..iSqrt (div p 2)]
  n <- [1..m-1]
  guard (mod m 2 == 0 || mod n 2 == 0)
  guard (gcd m n == 1)
  let a = m^2 - n^2
      b = 2*m*n
      c = m^2 + n^2
  guard (a+b+c <= p)
  return (min a b, max a b, c)

{-# SPECIALIZE pythagoreanTriplesWithPerimeterUpTo :: Int -> [(Int,Int,Int)] #-}
pythagoreanTriplesWithPerimeterUpTo :: (Integral a) => a -> [(a,a,a)]
pythagoreanTriplesWithPerimeterUpTo p =
  concat . map (expand p) . primitivePythagoreanTriplesWithPerimeterUpTo $ p
    where
      expand :: (Integral a) => a -> (a,a,a) -> [(a,a,a)]
      expand p (a,b,c) = [(m*a,m*b,m*c) | m <- [1..div p (a+b+c)]]

-- Continued fractions

{-# SPECIALIZE cfracSqrt :: Int -> ([Int],[Int]) #-}
cfracSqrt :: (Integral a) => a -> ([a],[a])
cfracSqrt n = cfracSpecialQuad n (iSqrt n) 0 1 [] []
  where
    cfracSpecialQuad n iSqrtn b d previous terms =
      case elemIndex (b,d) previous of
        Just i -> splitAt (length terms - i - 1) (reverse terms)
        Nothing ->
          cfracSpecialQuad n iSqrtn b' d' ((b,d):previous) (a:terms)
            where
              a = div (iSqrtn + b) d
              b' = a*d-b
              -- The following division is always exact
              -- Can prove by induction that d divides n - b^2
              -- and b' = b mod d, so d divides n - b'^2
              -- Base case works because initially d=1, b=0
              -- That's why this function is "Special"
              d' = div (n - b'^2) d

cfracSqrtFull :: (Integral a) => a -> [a]
cfracSqrtFull n = init ++ cycle pat
  where
    (init,pat) = cfracSqrt n

convergents :: (Integral a) => [a] -> [Ratio a]
convergents = convergents' (1%1,0%1,0%1,1%1)
  where
    convergents' f (n:ns) = applyMoebius f (n%1) : convergents' (composeMoebius f (n%1,1%1,1%1,0%1)) ns

-- Special sequences

-- Fib:   [1,1] -> [0,1] -> [0,1,1,2,3,5,8,13,21,34,55..]
-- Lucas: [1,1] -> [2,1] -> [2,1,3,4,7,11,18,29,47,76..]
-- Pell:  [1,2] -> [0,1] -> [0,1,2,5,12,29,70,169..]
recurrence :: (Num a) => [a] -> [a] -> [a]
recurrence cs xx@(x:xs) = x : recurrence cs (xs ++ [dot cs xx])
  where
    dot :: (Num a) => [a] -> [a] -> a
    dot xs ys = foldl (+) 0 . map (uncurry (*)) $ zip xs ys

fibonacci :: (Num a) => [a]
fibonacci = fibWith 0 1
  where
    fibWith a b = a : fibWith b (a+b)

polygonalNumbers :: (Integral a) => a -> [a]
polygonalNumbers r = [div (n*((r-2)*n-(r-4))) 2 | n <- [0..]]

triangularNumbers :: (Integral a) => [a]
triangularNumbers = [div (n * (n+1)) 2 | n <- [0..]]

abundant :: (Integral a) => [a]
abundant = filter isAbundant [2..]

{-# SPECIALIZE primes :: [Integer] #-}
{-# SPECIALIZE primes :: [Int] #-}
primes :: (Integral a) => [a]
primes = 2 : 3 : 5 : 7 : sieve (takeWhileIncreasing (spin wheel2357 11)) -- takeWhileIncreasing sucks up 0.67 seconds for first 1e6 primes
  where
    wheel2357 :: (Integral a) => [a]
    wheel2357 = 2:4:2:4:6:2:6:4:2:4:6:6:2:6:4:2:6:4:6:8:4:2:4:2:4:8:6:4:6:2:4:6:2:6:6:4:2:4:6:2:6:4:2:4:2:10:2:10:wheel2357

    spin :: (Integral a) => [a] -> a -> [a]
    spin (x:xs) n = n : spin xs (n+x)

    takeWhileIncreasing :: (Ord a) => [a] -> [a]
    takeWhileIncreasing (x:yy@(y:_))
      | x < y = x : takeWhileIncreasing yy
      | otherwise = [x]

    sieve :: (Integral a) => [a] -> [a]
    sieve (x:xs) = x : sieve' xs (insertprime x xs (Heap.empty :: Heap.MinPrioHeap a [a]))

    insertprime :: (Integral a) => a -> [a] -> Heap.MinPrioHeap a [a] -> Heap.MinPrioHeap a [a]
    insertprime p xs table
      | squareGood p = Heap.insert (p*p,map (*p) xs) table -- squareGood check sucks up 0.73 seconds for first 1e6 primes
      | otherwise = table

    squareGood :: (Integral a) => a -> Bool
    squareGood n = toInteger n^2 == toInteger (n^2)

    sieve' :: (Integral a) => [a] -> Heap.MinPrioHeap a [a] -> [a]
    sieve' [] _ = []
    sieve' xx@(x:xs) table
      | head == Nothing = xx -- this check takes no discernable time
      | n <  x  = sieve' xx (adjust table)
      | n == x  = sieve' xs (adjust table)
      | otherwise = x : sieve' xs (insertprime x xs table)
        where
          head = Heap.viewHead table
          Just (n,_) = head

    adjust :: (Integral a) => Heap.MinPrioHeap a [a] -> Heap.MinPrioHeap a [a]
    adjust table
      | n > n' = restOfTable -- this check costs 0.32 seconds for first 1e6 primes
      | otherwise = Heap.insert (n',ns) restOfTable
        where
          Just ((n,n':ns),restOfTable) = Heap.view table

-- Word games

alphabetScore :: Char -> Int
alphabetScore c
  | ord c >= ord 'A' && ord c <= ord 'Z' = ord c - ord 'A' + 1
  | ord c >= ord 'a' && ord c <= ord 'z' = ord c - ord 'a' + 1
  | otherwise = 0

-- Moebius functions

type Moebius a = (a,a,a,a)

composeMoebius :: Num a => Moebius a -> Moebius a -> Moebius a
composeMoebius (a,b,c,d) (e,f,g,h) = (a*e+b*g,a*f+b*h,c*e+d*g,c*f+d*h)

applyMoebius :: Fractional a => Moebius a -> a -> a
applyMoebius (a,b,c,d) x = (a*x+b)/(c*x+d)

-- extended Euclidean algorithm and modular inverse

egcd :: Integral a => a -> a -> (a,a,a)
egcd a b = egcd' a b 1 0 0 1
  where
    egcd' a b c d e f
      | a < 0 = egcd' (-a) b (-c) (-d) e f
      | b < 0 = egcd' a (-b) c d (-e) (-f)
      | a > b = egcd' b a e f c d
      | a == 0 = (b,e,f)
      | otherwise = egcd' (b `mod` a) a
                    (e - c * (b `div` a)) (f - d * (b `div` a)) c d

modularInverse :: Integral a => a -> a -> a
modularInverse p a = y'
  where
    (g,_,y) = egcd p a
    y' = if g == 1
         then y `mod` p
         else error $ "no modular inverse of " ++
              show a ++ " mod " ++ show p
