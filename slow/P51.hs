import Euler
import Control.Monad

nothingCombos :: [a] -> [[Maybe a]]
nothingCombos [] = [[]]
nothingCombos (x:xs) = (map (Nothing:) ncxs) ++ (map (Just x:) ncxs)
  where
    ncxs = nothingCombos xs

hasNothing :: Eq a => [Maybe a] -> Bool
hasNothing = or . map (==Nothing)

candidates :: [Maybe Int] -> [Int]
candidates md = do
  d <- if head md == Nothing then [1..9] else [0..9]
  let ds = [if x == Nothing then d else (\(Just y) -> y) x | x <- md]
  return . undigits $ ds

answer :: Int
answer = head $ do
  p <- primes
  md <- nothingCombos $ digits p
  guard $ hasNothing md
  let cs = candidates md
  guard $ (length . filter isPrime) cs >= 8
  return $ head cs

main :: IO ()
main = print answer
