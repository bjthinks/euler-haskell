import Euler

f :: Integer -> Integer
f = paste (+) id (undigits . reverse . digits)

isLychrel :: Integer -> Bool
isLychrel n
  | n <= 10000 = not . or . map isPalindrome . take 50 . tail . iterate f $ n
  | otherwise = error "isLychrel only known for n <= 10000"

answer :: Int
answer = length . filter isLychrel $ [1..9999]

main :: IO ()
main = print answer
