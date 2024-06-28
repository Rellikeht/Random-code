import qualified Data.ByteString.Char8 as B
import qualified Data.Word as W
import System.Environment
import System.IO

main = do
    prog <- getArgs
    input <- getContents
    execute (parse (head prog)) input

matchParens :: String -> Bool
matchParens s = helper 0 parens == 0
  where
    parens = filter (`elem` [ '[', ']' ]) s

    helper n "" = n
    helper n (c : rest)
        | c == '[' = helper (n + 1) rest
        | c == ']' = helper (n - 1) rest
        | otherwise = helper n rest

progChar :: Char -> Bool
progChar = flip elem [ '+', '-', '.', ',', '[', ']', '<', '>' ]

parse :: String -> Either String B.ByteString
parse s
    | matchParens s = Right $ B.pack $ filter progChar s
    | otherwise = Left "Brackets not matched"

execute :: Either String B.ByteString -> String -> IO ()
execute (Left err) _ = hPutStrLn stderr $ err ++ "\n"
execute (Right program) input = exec program input 0 [] (repeat 0) []

toW8 :: Char -> W.Word8
toW8 = toEnum . fromEnum

fromW8 :: W.Word8 -> Char
fromW8 = toEnum . fromEnum

exec :: B.ByteString
    -> String -> Int -> [ Int ] -> [ W.Word8 ] -> [ W.Word8 ] -> IO ()
exec program input position stack front back
    | position == B.length program = return ()
    | curinst == '.' = (putChar . fromW8 . head) front >> continue
    | otherwise = continue
  where
    continue = exec program input newpos newstack newfront newback

    curinst = B.index program position

    newpos
        | curinst == ']' && head front /= 0 = head stack + 1
        | otherwise = position + 1

    newstack = case curinst of
        ']' -> if head front == 0 then tail stack else stack
        '[' -> position : stack
        _ -> stack

    newback = case curinst of
        '>' -> head front : back
        '<' -> tail back
        _ -> back

    newfront = case curinst of
        '+' -> (head front + 1) : tail front
        '-' -> (head front - 1) : tail front
        ',' -> toW8 (head input) : tail front
        '>' -> tail front
        '<' -> head back : front
        _ -> front

    newinput = if curinst == ',' then tail input else input

-- import System.Exit
-- import System.IO

-- ??
-- execute (Left err) = hPutStrLn stderr err >> exitWith (exitFailure 1)
