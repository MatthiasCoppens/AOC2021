import Data.List
import Data.Maybe
import Data.Graph

type Coord = (Int, Int)

-- Find local minima in a grid
mins :: (Bounded a, Ord a) => [[a]] -> [a]
mins grid = catMaybes $ concat $
    (zipWith5 . zipWith5) f
    grid
    (map (maxBound :) grid)
    (map ((++ [maxBound]) . tail) grid)
    (repeat maxBound : grid) ((++ [repeat maxBound]) $ tail grid)
    where
        f x l r d u
            | minimum [l, r, d, u] > x = Just x
            | otherwise                = Nothing

-- List of coordinates of non-9 values
coords :: [[Int]] -> [Coord]
coords = catMaybes . concat .
    (zipWith3 . zipWith3) f (repeat [0 ..]) (map repeat [0 ..])
    where
        f _ _ 9 = Nothing
        f x y _ = Just (x, y)

-- Create a graph of where neighbouring coordinates are connected
createGraph :: [Coord] -> Graph
createGraph cs =
    let (graph, _, _) = graphFromEdges
            [ ((), (x, y), [(x-1, y), (x+1, y), (x, y-1), (x, y+1)])
            | (x, y) <- cs
            ]
    in  graph

parse :: String -> [[Int]]
parse = (map . map) (subtract (fromEnum '0') . fromEnum) . lines

solve1 :: [[Int]] -> Int
solve1 = sum . map succ . mins

solve2 :: [[Int]] -> Int
solve2 = product . take 3 . reverse . sort . map treeSize . components .
    createGraph . coords
    where
        treeSize (Node _ children) = succ $ sum $ map treeSize children

main :: IO ()
main = do
    input <- parse <$> getContents
    print $ solve1 input
    print $ solve2 input
