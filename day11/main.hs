type Grid = [[Word]]

-- Find fixed point for a function f, starting from a certain value
fix :: Eq a => (a -> a) -> (a -> a)
fix f = go
    where
        go x = case f x of
            x'  | x' == x   ->    x'
                | otherwise -> go x'

-- Level up octopuses next to flashing octopuses,
-- set energy level of flashing octopuses to 0
interactions :: Grid -> Grid
interactions grid =
    let l  = left  grid
        r  = right grid
        u  = up    grid
        d  = down  grid
        lu = left  u
        ld = left  d
        ru = right u
        rd = right d
    in  (zipWith . zipWith) flashed grid
            $ foldr ((zipWith . zipWith) interaction) grid
            [l, r, u, d, lu, ld, ru, rd]
    where
        left  = map (0 :)
        right = map ((++ [0]) . tail)
        up    = (repeat 0 :)
        down  = (++ [repeat 0]) . tail
        interaction _ 0 = 0
        interaction n m
            | n > 9     = succ m
            | otherwise =      m
        flashed n m
            | n > 9     = 0
            | otherwise = m

-- Simulation step
step :: Grid -> Grid
step = fix interactions . (map . map) succ

-- Parse input and run simulation steps,
-- gives infinite list of number of zeros per simulation step
zeros :: String -> [Int]
zeros = map (length . filter (== 0) . concat) . iterate step
            . (map . map) (read . (:[])) . lines

main :: IO ()
main = do
    input <- zeros <$> getContents
    print $ sum $ take 101 input
    print $ length $ takeWhile (< 100) input
