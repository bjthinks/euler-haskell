import Euler
import qualified Data.Heap as Heap

-- Heap.empty :: Heap.MinPrioHeap k v
-- Heap.insert :: (k,v) -> Heap.MinPrioHeap k v -> -> Heap.MinPrioHeap k v
-- Heap.view :: Heap.MinPrioHeap k v -> Maybe ((k,v),Heap.MinPrioHeap k v)
-- Heap.viewHead :: Heap.MinPrioHeap k v -> Maybe (k,v)

type H = Heap.MinPrioHeap Int ([Int],[Int])

concatenatablePrimeSets :: Int -> [[Int]]
concatenatablePrimeSets num = run initial
  where
    initial :: H
    initial = Heap.insert (heapItem [] (tail primes)) (Heap.empty :: H)

    run :: H -> [[Int]]
    run h
      | length ans == num = ans : run h1
      | and (map (compatibleWith p) (reverse ans)) = run h3
      | otherwise = run h2
        where
          h3 = Heap.insert (heapItem newans ps) h2
          h2 = Heap.insert (heapItem ans ps) h1
          newans = p:ans
          Just ((min,(ans,p:ps)),h1) = Heap.view h

    heapItem :: [Int] -> [Int] -> (Int,([Int],[Int]))
    heapItem ans ps = (mincost ans ps,(ans,ps))

    mincost :: [Int] -> [Int] -> Int
    mincost ans ps = sum ans + sum (take (num - length ans) ps)

compatibleWith :: Int -> Int -> Bool
compatibleWith p q = concatenationIsPrime p q && concatenationIsPrime q p

intlog10 :: Int -> Int
intlog10 n
  | n /= 0 = 1 + intlog10 (div n 10)
  | otherwise = 0

concatenationIsPrime :: Int -> Int -> Bool
concatenationIsPrime a b = isPrime $ a * 10^(intlog10 b) + b

answer :: Int
answer = sum $ head $ concatenatablePrimeSets 5

main :: IO ()
main = putStrLn $ show answer
