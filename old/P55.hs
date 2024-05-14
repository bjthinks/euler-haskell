reverseNumber :: Integer -> Integer
reverseNumber = read . reverse . show

lychrelStep :: Integer -> Integer
lychrelStep n = n + reverseNumber n

lychrelSeries :: Integer -> [Integer]
lychrelSeries = tail . iterate lychrelStep

isLychrel :: Integer -> Bool
isLychrel = or . map isPalindrome . take 50 . lychrelSeries

isPalindrome :: Integer -> Bool
isPalindrome n = show n == reverse (show n)

main :: IO ()
main = (putStrLn . show . length . filter id . map (not . isLychrel)) [1..9999]
