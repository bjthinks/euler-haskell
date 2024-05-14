import Data.Char

sumSquareDigits :: Integer -> Integer
sumSquareDigits = sum . map ((^2) . toInteger . (\x -> ord x - ord '0')) . show

numberChain :: Integer -> [Integer]
numberChain = iterate sumSquareDigits

is89 :: [Integer] -> Bool
is89 (x:xs)
  | x == 89 = True
  | x == 1 = False
  | otherwise = is89 xs

main :: IO ()
main = (putStrLn . show . length . filter (is89 . numberChain))
       [1..9999999]
