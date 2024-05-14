monthLengths :: Int -> [Int]
monthLengths y
  | mod y 4 == 0 && (mod y 100 /= 0 || mod y 400 == 0) =
    [31,29,31,30,31,30,31,31,30,31,30,31]
  | otherwise =
    [31,28,31,30,31,30,31,31,30,31,30,31]

allMonthLengths :: [Int]
allMonthLengths = concat . map monthLengths $ [1901..2000]

dayOfWeeks :: [Int]
dayOfWeeks = scanl (\w m -> mod (m+w) 7) (mod 366 7) (init allMonthLengths)

answer :: Int
answer = length . filter (==0) $ dayOfWeeks

main :: IO ()
main = print answer

