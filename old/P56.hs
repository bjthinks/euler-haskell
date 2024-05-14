import Data.Char

numbers :: [Integer]
numbers = [a^b | a <- [1..99], b <- [1..99]]

digitSum :: Integer -> Int
digitSum = sum . map (\c -> ord c - ord '0') . show

main :: IO ()
main = (putStrLn . show . foldr1 max . map digitSum) numbers
