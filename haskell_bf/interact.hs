main = interact func

num = 5e4

-- func _ = (show . fact . round) num ++ "\nabc\n" ++ (show . fact . round)
--     (num + 2)

-- reverse waits
-- func _ = reverse $ p1 ++ p2 ++ "\nabc\n" ++ p3 ++ p4 ++ "def\n"

func _ = p1 ++ p2 ++ "\nabc\n" ++ p3 ++ p4 ++ "def\n"
  where
    f = get . fv . fact . round

    p1 = f num

    p2 = f 10

    p3 = f (num + 1)

    p4 = f 5

fact n = helper n 1
  where
    helper 0 a = a
    helper 1 a = a
    helper n a = helper (n - 1) (n * a)

fv n
    | n < 10000000 = Right n
    | otherwise = Left "Too big"

get (Right n) = "Result: " ++ show n ++ "\n"
get (Left s) = "Error: " ++ s ++ "\n"
