import qualified Data.ByteString.Char8 as B

main = interact func

num = 5e4

func _ = B.unpack $ foldl B.append B.empty elems
  where
    f = B.pack . get . fv . fact . round

    p1 = f num

    p2 = f 10

    p3 = f (num + 1)

    p4 = f 5

    e1 = B.pack "\nabc\n"

    e2 = B.pack "\ndef\n"

    elems = [ p1, p2, e1, p3, p4, e2 ]

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
