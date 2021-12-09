import Data.List.Split
import qualified Data.Map.Strict as M
import qualified Data.Set as S

type Segment = S.Set Char

untangle :: [Segment] -> M.Map Char Char
untangle segments =
    let m = foldr (\s -> M.insert (S.size s) s) M.empty segments
        m' = M.foldrWithKey
                (\c n -> M.insertWith S.union n $ S.singleton c) M.empty
            $ foldr (\c -> M.insertWith (+) c 1) M.empty
            $ concat $ map S.toList segments
        num1 = m M.! 2
        num4 = m M.! 4
        num7 = m M.! 3
        num8 = m M.! 7
        cf = num1
        bd = num4 S.\\ num1
        eg = num8 S.\\ S.unions [num1, num4, num7]
        [a] = S.toList $ num7 S.\\ num1
        [b] = S.toList $ bd `S.intersection` (m' M.! 6)
        [c] = S.toList $ cf `S.intersection` (m' M.! 8)
        [d] = S.toList $ bd `S.intersection` (m' M.! 7)
        [e] = S.toList $ eg `S.intersection` (m' M.! 4)
        [f] = S.toList $ cf `S.intersection` (m' M.! 9)
        [g] = S.toList $ eg `S.intersection` (m' M.! 7)
    in  M.fromList $ zip [a,b,c,d,e,f,g] ['a' ..]

decode :: [Segment] -> [Segment] -> [Segment]
decode segs = (map . S.map) (untangle segs M.!)

decodeNum :: [Segment] -> Int
decodeNum =
    let dict = M.fromList $ zip (map S.fromList [
                "abcefg",
                "cf",
                "acdeg",
                "acdfg",
                "bcdf",
                "abdfg",
                "abdefg",
                "acf",
                "abcdefg",
                "abcdfg"
            ]) [0 ..]
    in  foldl1 (\x y -> 10*x + y) . map (dict M.!)

parse :: String -> [([Segment], [Segment])]
parse = map (
        (\[l, r] -> (l, r)) . splitOn [S.singleton '|'] . map S.fromList . words
    ) . lines

solve1 :: [([Segment], [Segment])] -> Int
solve1 = sum . map (length . filter (`elem` [2,3,4,7]) . map S.size . snd)

solve2 :: [([Segment], [Segment])] -> Int
solve2 = sum . map (\(l, r) -> decodeNum $ decode l r)

main :: IO ()
main = do
    input <- parse <$> getContents
    print $ solve1 input
    print $ solve2 input
