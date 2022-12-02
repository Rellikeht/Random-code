module Fractions(Fraction(Frac)) where

import Data.Ratio

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

instance Show Fraction where
 show f = showF f

instance Read Fraction where
 readsPrec _ f = readsF f

instance Eq Fraction where
 (==) f1 f2 = eq f1 f2

instance Num Fraction where
 (+) f1 f2 = addF f1 f2
 (-) f1 f2 = subF f1 f2
 (*) f1 f2 = mulF f1 f2
 abs f = absF f
 signum f = sigF f
 fromInteger n = Frac (fromInteger n) 1

instance Fractional Fraction where
 (/) f1 f2 = divF f1 f2
 recip f = recipF f
 fromRational n = conv n

instance Ord Fraction where
 (<=) f1 f2 = leF f1 f2
 (<) f1 f2 = lF f1 f2

instance Real Fraction where
 toRational f = ratF f

-------------------------------------------------------------------------------
-- Operations on one fraction
-------------------------------------------------------------------------------

short f
 | g == 1 = f
 | otherwise =  Frac (quot n g) (quot d g)
  where
   n = num f
   d = den f
   g = mygcd n d

recipF (Frac n d) = short $ Frac d n

absF (Frac n d) = Frac (abs n) (abs d)
sigF (Frac n d) = Frac (signum n) 1

-------------------------------------------------------------------------------
-- Operations on two fractions
-------------------------------------------------------------------------------

addF (Frac n1 d1) (Frac n2 d2) = short $ Frac n d
 where
  g = mygcd d1 d2
  d = d1*d2
  n = n1*d2 + n2*d1

subF s (Frac n d) = addF s (Frac (-n) d)

mulF f1 f2 = short $ Frac n d
 where
  n = num f1 * num f2
  d = den f1 * den f2

divF f1 f2 = mulF f1 (recipF f2)
powF (Frac n d) i = Frac (n^i) (d^i)

eq f1 f2 = eqs (short f1) (short f2)

eqs f1 f2 = (n1 == n2) && (d1 == d2)
 where
  n1 = num f1
  n2 = num f2
  d1 = den f1
  d2 = den f2

lF (Frac n1 d1) (Frac n2 d2) = c1 < c2
 where
  c1 = fromIntegral n1 * fromIntegral d2
  c2 = fromIntegral n2 * fromIntegral d1

leF f1 f2 = (f1 == f2) || (lF f1 f2)

-------------------------------------------------------------------------------
-- Representation
-------------------------------------------------------------------------------

showF (Frac n d) = show n ++ "/" ++ show d

readsF s = [(short $ Frac dn dd, leftover)]
 where
  dn = read (takeWhile isnum s)
  butfirst = dropWhile isnum s
  rest = dropWhile nisnum butfirst
  dd = read (takeWhile isnum rest)
  leftover = dropWhile isnum rest
  isnum ch = (code >= 48) && (code <= 57)
   where code = fromEnum ch
  nisnum ch = not $ isnum ch

conv n = short $ Frac i p
 where
  v = fromRational n
  l = round (logBase 10 v)
  p = 10^l
  i = round $ fromIntegral p * v

ratF :: Fraction -> Rational
ratF (Frac n d) = fromIntegral n % fromIntegral d
