import Euler

isSumFacDigits :: Int -> Bool
isSumFacDigits = equal id $ sum . map factorial . digits

answer :: Int
answer = sum $ filter isSumFacDigits [3..7 * factorial 9]

main :: IO ()
main = putStrLn $ show answer
