module Fractions(Fraction(Frac)) where

import Data.Ratio

-------------------------------------------------------------------------------
-- Definition and instantiation
-------------------------------------------------------------------------------

-- typedef struct {
-- 	long long num, den;
-- } Fraction;
--
-- Fraction Frac(long long d, long long d) {
-- 	return Fraction{n = num, d = den};
-- }
-- :)

data Fraction = Frac {num, den :: Int}

-------------------------------------------------------------------------------
-- Representation
-------------------------------------------------------------------------------

instance Show Fraction where
 show (Frac n d) = show n ++ "/" ++ show d

instance Read Fraction where
 readsPrec _ s = [(short $ Frac dn dd, leftover)]
  where
   dn = read (takeWhile isnum s)
   butfirst = dropWhile isnum s
   rest = dropWhile nisnum butfirst
   dd = read (takeWhile isnum rest)
   leftover = dropWhile isnum rest
   isnum ch = (code >= 48) && (code <= 57)
    where code = fromEnum ch
   nisnum ch = not $ isnum ch

instance Real Fraction where
 toRational (Frac n d) = fromIntegral n % fromIntegral d

instance Eq Fraction where
 (==) f1 f2 = eqs (short f1) (short f2)

instance Num Fraction where
 (+) (Frac n1 d1) (Frac n2 d2) = short $ Frac n d
  where
   g = mygcd d1 d2
   d = d1*d2
   n = n1*d2 + n2*d1
 (-) s (Frac n d) = s+Frac (-n) d
 (*) f1 f2 = short $ Frac n d
  where
   n = num f1 * num f2
   d = den f1 * den f2
 abs (Frac n d) = Frac (abs n) (abs d)
 signum (Frac n d) = Frac (signum n) 1
 fromInteger n = Frac (fromInteger n) 1

powF (Frac n d) i = Frac (n^i) (d^i)

instance Fractional Fraction where
 (/) f1 f2 = f1 * recip f2
 recip (Frac n d) = short $ Frac d n
 fromRational n = short $ Frac i p
  where
   v = fromRational n
   l = round (logBase 10 v)
   p = 10^l
   i = round $ fromIntegral p * v


instance Ord Fraction where
 (<=) f1 f2 = (f1 == f2) || f1 < f2
 (<) (Frac n1 d1) (Frac n2 d2) = c1 < c2
  where
   c1 = fromIntegral n1 * fromIntegral d2
   c2 = fromIntegral n2 * fromIntegral d1

short f
 | g == 1 = f
 | otherwise =  Frac (quot n g) (quot d g)
  where
   n = num f
   d = den f
   g = mygcd n d

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------

mygcd a b
 | b == 0 = a
 | otherwise = mygcd b (mod a b)

mylcm a b = quot (fromIntegral m) g
 where
  m = fromInteger a * fromInteger b
  g = mygcd a b


eqs f1 f2 = (n1 == n2) && (d1 == d2)
 where
  n1 = num f1
  n2 = num f2
  d1 = den f1
  d2 = den f2
