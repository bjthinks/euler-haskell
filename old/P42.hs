import IsPrime
import Data.Char

isTriangular :: Int -> Bool
isTriangular n = n == s * (s+1) `div` 2
  where
    s = iSqrt (n * 2)

wordValue :: String -> Int
wordValue = sum . map (\x -> ord x - ord 'A' + 1)

getWords :: IO [String]
getWords = do blet <- getLine
              return . read $ "[" ++ blet ++ "]"

main :: IO ()
main = do ws <- getWords
          putStrLn . show . length . filter id $
            map (isTriangular . wordValue) ws
