import qualified Data.ByteString.Char8 as B
import qualified Data.Word as W
import System.Environment

main = do
    prog <- getArgs
    interact $ execute (parse (head prog))

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
    | otherwise = Left "Parens not matched"

execute :: Either String B.ByteString -> String -> String
execute (Left err) _ = err ++ "\n"
execute (Right program) input = exec program input 0 [] ""

toW8 :: Char -> W.Word8
toW8 = toEnum . fromEnum

fromW8 :: W.Word8 -> Char
fromW8 = toEnum . fromEnum

exec :: B.ByteString -> String -> Int -> [ Int ] -> String -> String
exec program input position stack output
    | position == B.length program = reverse output
    | otherwise = exec program input newpos newstack newout
  where
    newpos = position + 1

    newstack = stack

    newout = ""

-- newout = if index program position 

-- import System.Exit
-- import System.IO


-- ??
-- execute (Left err) = hPutStrLn stderr err >> exitWith (exitFailure 1)
