import Data.Char

f :: Int -> Int
f = sum . map fac . map (\x -> ord x - ord '0') . show
  where
    fac 0 = 1
    fac n = n * fac (n-1)

nonLoopLen :: Int -> Int
nonLoopLen n
  | n == f n = 1
  | n == 871 = 2
  | n == 872 = 2
  | n == 45361 = 2
  | n == 45362 = 2
  | n == 169 = 3
  | n == 363601 = 3
  | n == 1454 = 3
  | otherwise = 1 + nonLoopLen (f n)

answer :: Int
answer = (length . filter (==60) . map nonLoopLen) [1..999999]

main :: IO ()
main = (putStrLn . show) answer
