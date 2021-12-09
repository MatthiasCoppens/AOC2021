import Data.List.Split
import qualified Data.Set as S

type Segment = S.Set Char

parse :: String -> [([Segment], [Segment])]
parse = map ((\[l, r] -> (x, y)) . splitOn [S.singleton '|'] . map S.fromList . words) . lines

solve1 :: [Segment] -> Int
solve1 = length . filter (`elem` [2,3,4,7]) . map S.size

main :: IO ()
main = do
    (l, r) <- parse <$> getContents
    print $ solve1 r
