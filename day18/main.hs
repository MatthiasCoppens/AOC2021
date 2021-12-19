parseNum :: String -> [(Int, Int)]
parseNum = go 0
    where
        go depth (c : cs) = case c of
            '[' ->                     go (succ depth) cs
            ']' ->                     go (pred depth) cs
            ',' ->                     go       depth  cs
            _   -> (depth, read [c]) : go       depth  cs
        go _ [] = []

explode, split, reduce :: [(Int, Int)] -> [(Int, Int)]

explode ts = init $ tail $ go $ (0, 0) : ts ++ [(0, 0)]
    where
        go ((dl, ll) : (5, l) : (5, r) : (dr, rr) : rest)
            = (dl, ll + l) : (4, 0) : (dr, r + rr) : rest
        go (t : ts) = t : go ts
        go [] = []

split ((d, x) : rest)
    | x > 9 =
        let x' = x `div` 2
        in  (d + 1, x') : (d + 1, x - x') : rest
    | otherwise = (d, x) : split rest
split [] = []

reduce x =
    let x'  = explode x
        x'' = split   x
    in  if x /= x' then reduce x' else if x /= x'' then reduce x'' else x

add :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
add l r = reduce [(d + 1, x) | tree <- [l, r], (d, x) <- tree]

magnitude :: [(Int, Int)] -> Int
magnitude [(0, m)] = m
magnitude tree = magnitude $ go 0 tree
    where
        go d ((dl, xl) : (dr, xr) : rest) | (dl == dr) && (dl > d)
            = (dl - 1, 3 * xl + 2 * xr) : rest
        go d (t : ts) = t : go d ts
        go _ [] = error $ show tree

main :: IO ()
main = do
    inputs <- map parseNum . lines <$> getContents
    print $ magnitude $ foldl1 add inputs
    print $ maximum [magnitude (add x y) | x <- inputs, y <- inputs]
