import qualified Data.Map.Strict as M

stop10 :: Word -> Word
stop10 = head . dropWhile (> 10) . iterate (subtract 10)

solve1 :: Word -> Word -> Word
solve1 = go [1+2+3, 4+5+6 ..] 1 0 0
    where
        go (d : ds) n s1 s2 p1 p2 =
            let p1' = stop10 $ p1 + d
                s1' = s1 + p1'
            in  if s1' >= 1000 then 3 * n * s2 else go ds (succ n) s2 s1' p2 p1'

solve2 :: Word -> Word -> Word
solve2 p1 p2 = maximum $ go $ M.singleton (True, p1, p2, 0, 0) 1
    where
        go m = case M.minViewWithKey m of
            Nothing -> M.empty
            Just (((b, p1, p2, s1, s2), n), m')
                | s2 >= 21  -> M.insertWith (+) b n $ go m'
                | otherwise -> go $ M.unionWith (+) m' $ M.fromListWith (+)
                    [ ((not b, p2, p1', s2, s1 + p1'), n)
                    | d1 <- [1 .. 3]
                    , d2 <- [1 .. 3]
                    , d3 <- [1 .. 3]
                    , let p1' = stop10 $ p1 + d1 + d2 + d3
                    ]

main :: IO ()
main = do
    p1 <- readLn
    p2 <- readLn
    print $ solve1 p1 p2
    print $ solve2 p1 p2
