{-# LANGUAGE LambdaCase #-}

import Data.Maybe
import Data.Tuple
import qualified Data.Map.Strict as M

type Coord = (Int, Int)

path :: M.Map Coord Int -> Coord -> Coord -> Int
path costs start end = go M.empty $ M.singleton (0, 0) 0
    where
        (end, _) = M.findMax costs
        go visited current = case current M.!? end of
            Just n  -> n
            Nothing ->
                let (n, (x, y)) = minimum $ map swap $ M.toList current
                    visited' = M.insert (x, y) n visited
                    neighbours = M.fromList
                        [ (c, n + n')
                        | c <- [(x+1, y), (x-1, y), (x, y+1), (x, y-1)]
                        , let m = costs M.!? c
                        , isJust m
                        , let Just n' = m
                        ] M.\\ visited M.\\ current
                    current' = M.union neighbours $ M.delete (x, y) current
                in  go visited' current'

addOne :: M.Map Coord Int -> M.Map Coord Int
addOne = M.map $ \case
    9 ->      1
    n -> succ n
down, right, grow :: Int -> M.Map Coord Int -> M.Map Coord Int
down  d = M.mapKeysMonotonic $ \(x, y) -> (x, y + d)
right d = M.mapKeysMonotonic $ \(x, y) -> (x + d, y)
grow d =
      M.unions . take 5 . iterate (down  d . addOne)
    . M.unions . take 5 . iterate (right d . addOne)

parse :: String -> M.Map Coord Int
parse = M.map (read . (:[])) . M.fromList . concat
    . (zipWith zip) ((zipWith zip) (repeat [0 ..]) (map repeat [0 ..]))
    . takeWhile (not . null) . lines

solve :: M.Map Coord Int -> Int
solve grid =
    let (end, _) = M.findMax grid
    in  path grid (0, 0) end

main :: IO ()
main = do
    input <- parse <$> getContents
    print $ solve            input
    print $ solve $ grow 100 input
