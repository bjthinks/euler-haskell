import Data.List (sort)
import Data.Bits (Bits, xor)
import System.IO
import Data.Array

hi :: Int
hi = 20001

fastmex :: (Integral a) => [a] -> a
fastmex v = fastmex' v (False,False,False,False,False,False,False,False,False,False)
fastmex' :: (Integral a) => [a] -> (Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool,Bool) -> a
fastmex' (0:xs) (a,b,c,d,e,f,g,h,i,j) = fastmex' xs (True,b,c,d,e,f,g,h,i,j)
fastmex' (1:xs) (a,b,c,d,e,f,g,h,i,j) = fastmex' xs (a,True,c,d,e,f,g,h,i,j)
fastmex' (2:xs) (a,b,c,d,e,f,g,h,i,j) = fastmex' xs (a,b,True,d,e,f,g,h,i,j)
fastmex' (3:xs) (a,b,c,d,e,f,g,h,i,j) = fastmex' xs (a,b,c,True,e,f,g,h,i,j)
fastmex' (4:xs) (a,b,c,d,e,f,g,h,i,j) = fastmex' xs (a,b,c,d,True,f,g,h,i,j)
fastmex' (5:xs) (a,b,c,d,e,f,g,h,i,j) = fastmex' xs (a,b,c,d,e,True,g,h,i,j)
fastmex' (6:xs) (a,b,c,d,e,f,g,h,i,j) = fastmex' xs (a,b,c,d,e,f,True,h,i,j)
fastmex' (7:xs) (a,b,c,d,e,f,g,h,i,j) = fastmex' xs (a,b,c,d,e,f,g,True,i,j)
fastmex' (8:xs) (a,b,c,d,e,f,g,h,i,j) = fastmex' xs (a,b,c,d,e,f,g,h,True,j)
fastmex' (9:xs) (a,b,c,d,e,f,g,h,i,j) = fastmex' xs (a,b,c,d,e,f,g,h,i,True)
fastmex' (_:xs) (a,b,c,d,e,f,g,h,i,j) = fastmex' xs (a,b,c,d,e,f,g,h,i,j)
fastmex' [] (False,_,_,_,_,_,_,_,_,_) = 0
fastmex' [] (True,False,_,_,_,_,_,_,_,_) = 1
fastmex' [] (True,True,False,_,_,_,_,_,_,_) = 2
fastmex' [] (True,True,True,False,_,_,_,_,_,_) = 3
fastmex' [] (True,True,True,True,False,_,_,_,_,_) = 4
fastmex' [] (True,True,True,True,True,False,_,_,_,_) = 5
fastmex' [] (True,True,True,True,True,True,False,_,_,_) = 6
fastmex' [] (True,True,True,True,True,True,True,False,_,_) = 7
fastmex' [] (True,True,True,True,True,True,True,True,False,_) = 8
fastmex' [] (True,True,True,True,True,True,True,True,True,False) = 9
fastmex' _ (True,True,True,True,True,True,True,True,True,True) = error "please extend fastmex"

mex :: (Integral a) => [a] -> a
mex v = mex' 0 $ sort v
mex' :: (Integral a) => a -> [a] -> a
mex' v [] = v
mex' v (x:xs)
  | v == x = mex' (v+1) xs
  | v > x  = mex' v xs
  | v < x  = v

nimVal :: Int -> Int
nimVal n = fastmex [xor (table ! k) (table ! (n-2-k)) | k <- [0..n-2]]

table :: Array Int Int
table = listArray (0,hi) [nimVal x | x <- [0..hi]]

answer :: Int
answer = (length . filter (/=0) . tail . elems) table

main :: IO ()
main = do hSetBuffering stdout NoBuffering
          putStrLn $ show $ map (table !) [0..hi]
          putStrLn $ show answer
