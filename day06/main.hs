import Data.List.Split

-- [n0, n1, .., n7, n8] -> [n1, n2, .., n5, n6, n7 + n0, n8, n0]
next :: [Word] -> [Word]
next (n : ns) =
    let (ns', (n' : n'' : _)) = splitAt 6 ns
    in  ns' ++ [n' + n, n'', n]

-- Parse as frequency list: "1,2,3,2,3" -> [0,1,2,2,0,0,0,0,0]
parse :: String -> [Word]
parse = foldr f (replicate 9 0) . map read . splitOn "," . head . lines
    where
        f i ns =
            let (ns', (n : ns'')) = splitAt i ns
            in  ns' ++ succ n : ns''

main :: IO ()
main = do
    days <- iterate next . parse <$> getContents
    mapM_ (print . sum . (days !!)) [80, 256]
