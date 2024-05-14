import Euler
import Control.Monad
import Data.List

--      a
--       ----
--           b      c
--       ---- ---- /
--      j         d
--  v~~^ \       /
-- i      h --- f --- e
--         \
--          g

solutions :: [[Int]]
solutions = do let nums = [1..10]
               a <- nums
               let a' = filter (/=a) nums
               b <- a'
               let b' = filter (/=b) a'
               d <- b'
               let d' = filter (/=d) b'
               let total = a+b+d
               c <- d'
               let c' = filter (/=c) d'
               let f = total-c-d
               guard (elem f c')
               let f' = filter (/=f) c'
               e <- f'
               let e' = filter (/=e) f'
               let h = total-e-f
               guard (elem h e')
               let h' = filter (/=h) e'
               g <- h'
               let g' = filter (/=g) h'
               let j = total-g-h
               guard (elem j g')
               let j' = filter (/=j) g'
               i <- j'
               let i' = filter (/=i) j'
               guard (i+j+b==total)
               return $ concat $ minFirst [[a,b,d],[c,d,f],[e,f,h],[g,h,j],[i,j,b]]

-- Cyclically permute a list so that the smallest element comes first
minFirst :: (Ord a) => [a] -> [a]
minFirst xs = zs ++ ys
  where
    (ys,zs) = splitAt minIndex xs
    Just minIndex = elemIndex (minimum xs) xs

answer :: Integer
answer = maximum $ map undigits $ filter ((==16) . length) $ map concat $ map (map digits) $ solutions

main :: IO ()
main = putStrLn $ show answer
