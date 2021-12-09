import Data.List
import Data.Maybe

type Board = [[Maybe Int]]

hasWon :: Board -> Bool
hasWon board = or $ map (and . map isNothing) $ board ++ transpose board

anyWon :: [Board] -> Maybe Board
anyWon = listToMaybe . filter hasWon

remove :: Int -> Maybe Int -> Maybe Int
remove x (Just y) | x == y = Nothing
remove _ m                 = m

crossOff :: Int -> [Board] -> [Board]
crossOff x = (map . map . map) (remove x)

parse :: String -> ([Int], [Board])
parse = go . lines
    where
        go (l : ls) = (read ('[' : l ++ "]"), go' ls)
        go' (_ : l1 : l2 : l3 : l4 : l5 : ls) =
            map (map (Just . read) . words) [l1, l2, l3, l4, l5] : go' ls
        go' _ = []

solve1 :: [Int] -> [Board] -> Int
solve1 (x : xs) boards =
    let boards' = crossOff x boards
    in  case anyWon boards' of
            Just board -> x * sum (map (sum . map (fromMaybe 0)) board)
            Nothing    -> solve1 xs boards'

solve2 :: [Int] -> [Board] -> Int
solve2 (x : xs) boards =
    let boards' = crossOff x boards
    in  case filter hasWon boards' of
            []      -> solve2 xs boards'
            winning -> case boards of
                [_] -> x * sum (map (sum . map (fromMaybe 0)) $ head winning)
                _   -> solve2 xs $ boards' \\ winning

main :: IO ()
main = do
    (xs, boards) <- parse <$> getContents
    print $ solve1 xs boards
    print $ solve2 xs boards
