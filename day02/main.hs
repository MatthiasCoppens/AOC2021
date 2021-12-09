type Command = (Direction, Int)
data Direction = Forward | Down | Up

parse :: String -> [Command]
parse = map f . lines
    where
        f s = case words s of
            ['f' : _, x] -> (Forward, read x)
            ['d' : _, x] -> (Down,    read x)
            ['u' : _, x] -> (Up,      read x)

solve1 :: [Command] -> Int
solve1 = go 0 0
    where
        go pos dep commands = case commands of
            []                    -> pos * dep
            ((Forward, x) : rest) -> go (pos + x)  dep      rest
            ((Down,    x) : rest) -> go  pos      (dep + x) rest
            ((Up,      x) : rest) -> go  pos      (dep - x) rest

solve2 :: [Command] -> Int
solve2 = go 0 0 0
    where
        go pos dep aim commands = case commands of
            []                    -> pos * dep
            ((Forward, x) : rest) -> go (pos + x) (dep + x * aim)  aim      rest
            ((Down,    x) : rest) -> go  pos       dep            (aim + x) rest
            ((Up,      x) : rest) -> go  pos       dep            (aim - x) rest

main :: IO ()
main = do
    input <- parse <$> getContents
    print $ solve1 input
    print $ solve2 input
