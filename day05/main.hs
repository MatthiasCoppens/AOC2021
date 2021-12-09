import qualified Data.Map.Strict as M
import qualified Data.Set        as S
import Text.ParserCombinators.ReadP

type X = Int
type Coord = (X, X)
type Line = (Coord, Coord)

num :: ReadP X
num = read <$> (many1 $ foldl1 (+++) $ map char ['0' .. '9'])

coord :: ReadP Coord
coord = (,) <$> (num <* char ',') <*> num

line :: ReadP Line
line = (,) <$> (coord <* string " -> ") <*> coord

straight :: Line -> Bool
straight ((x1, y1), (x2, y2)) = (x1 == x2) || (y1 == y2)

lineToSet :: Line -> S.Set Coord
lineToSet (c1, c2) | c1 > c2 = lineToSet (c2, c1)
lineToSet ((x1, y1), (x2, y2))
    | x1 == x2  = S.fromDistinctAscList [(x1, y) | y <- [y1 .. y2]]
    | y1 == y2  = S.fromDistinctAscList [(x, y1) | x <- [x1 .. x2]]
    | y1 <  y2  = S.fromDistinctAscList $ zip [x1 .. x2] [y1         .. y2]
    | otherwise = S.fromDistinctAscList $ zip [x1 .. x2] [y1, y1 - 1 .. y2]

freqs :: (Foldable t, Ord a) => t (S.Set a) -> M.Map a Word
freqs = foldr (M.unionWith (+) . M.fromSet (const 1)) M.empty

parse :: String -> [Line]
parse s = fst $ last $ readP_to_S (sepBy line (char '\n')) s

solve1 :: [Line] -> Int
solve1 = solve2 . filter straight

solve2 :: [Line] -> Int
solve2 = M.size . M.filter (> 1) . freqs . map lineToSet

main :: IO ()
main = do
    input <- parse <$> getContents
    print $ solve1 input
    print $ solve2 input
