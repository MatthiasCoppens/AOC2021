import Data.List.Split

parse :: String -> [Int]
parse = map read . splitOn "," . head . lines

solve :: [Int] -> [Int] -> Int
solve ds ns = minimum $ take (maximum ns) $ foldl1 (zipWith (+)) $ map f ns
    where
        f n = reverse (take n ds) ++ 0 : ds

main :: IO ()
main = do
    input <- parse <$> getContents
    print $ solve             [1 ..]  input
    print $ solve (scanl1 (+) [1 ..]) input
