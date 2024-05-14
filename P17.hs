w :: Int -> String
w 1 = "one"
w 2 = "two"
w 3 = "three"
w 4 = "four"
w 5 = "five"
w 6 = "six"
w 7 = "seven"
w 8 = "eight"
w 9 = "nine"
w 10 = "ten"
w 11 = "eleven"
w 12 = "twelve"
w 13 = "thirteen"
w 14 = "fourteen"
w 15 = "fifteen"
w 16 = "sixteen"
w 17 = "seventeen"
w 18 = "eighteen"
w 19 = "nineteen"
w 20 = "twenty"
w 30 = "thirty"
w 40 = "forty"
w 50 = "fifty"
w 60 = "sixty"
w 70 = "seventy"
w 80 = "eighty"
w 90 = "ninety"
w 100 = "onehundred"
w 200 = "twohundred"
w 300 = "threehundred"
w 400 = "fourhundred"
w 500 = "fivehundred"
w 600 = "sixhundred"
w 700 = "sevenhundred"
w 800 = "eighthundred"
w 900 = "ninehundred"
w 1000 = "onethousand"
w n
  | n < 100 = w (n - mod n 10) ++ w (mod n 10)
  | otherwise = w (n - mod n 100) ++ "and" ++ w (mod n 100)

answer :: Int
answer = sum $ map (length . w) [1..1000]

main :: IO ()
main = putStrLn $ show answer
