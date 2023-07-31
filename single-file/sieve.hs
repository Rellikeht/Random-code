import System.Environment
import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad
import Control.Monad.ST

primes :: Int -> UArray Int Bool
primes n = runSTUArray $ do
 sieve <- newArray (2, n) True
 forM_ [4,6..n] $ \i -> do
  writeArray sieve i False
 forM_ [3,5..n] $ \i -> do
  isPrime <- readArray sieve i
  if isPrime
  then
   do
    let start = i*i
    let step = 2*i
    forM_ [start,(start+step)..n] $ \j -> do writeArray sieve j False
  else
   pure ()
 return sieve

printPrime :: Int -> Int -> UArray Int Bool -> IO ()
printPrime i max arr
 | i > max = pure ()
 | arr!i = do
  print i
  next
 | otherwise = do
  next
  where
   next = printPrime (i+1) max arr

main = do
 args <- getArgs
 let num = read $ head args :: Int
 let prs = primes num
 printPrime 2 num prs
-- let last = prs!(num-2)
-- print last
