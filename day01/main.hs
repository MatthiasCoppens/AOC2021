import Data.List

parse :: String -> [Int]
parse = map read . lines

solve1 :: [Int] -> Int
solve1 ns@(_:ns') = length $ filter id $ zipWith (<) ns ns'

solve2 :: [Int] -> Int
solve2 ns@(_:ns'@(_:ns'')) = solve1 $ zipWith3 f ns ns' ns''
    where
        f x y z = x + y + z

main :: IO ()
main = do
    input <- parse <$> getContents
    print $ solve1 input
    print $ solve2 input
