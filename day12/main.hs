import Data.Char
import Data.List.Split
import qualified Data.Map.Strict as M
import qualified Data.Set        as S


type Graph a = M.Map a (S.Set a)

empty :: Graph a
empty = M.empty

addEdge :: Ord a => a -> a -> Graph a -> Graph a
addEdge x y = M.unionWith S.union $ M.fromList
    [(x, S.singleton y), (y, S.singleton x)]

removeVertex :: Ord a => a -> Graph a -> Graph a
removeVertex x = M.map (S.delete x) . M.delete x

fromList :: (Foldable t, Ord a) => t (a, a) -> Graph a
fromList = foldr (uncurry addEdge) empty

neighbours :: Ord a => Graph a -> a -> [a]
neighbours graph x = S.toList $ graph M.! x


parse :: String -> Graph String
parse s = fromList [(x, y) | [x, y] <- map (splitOn "-") (lines s)]

solve1 :: Graph String -> Word
solve1 = go "start"
    where
        go "end"   graph = 1
        go current graph =
            let graph' = if isLower (head current)
                    then removeVertex current graph
                    else graph
            in  sum $ map (flip go graph') $ neighbours graph current

-- Code replication, don't care
solve2 :: Graph String -> Word
solve2 graph = go S.empty "start"
    where
        go _       "end"   = 1
        go visited "start" | "start" `S.member` visited = 0
        go visited current | current `S.member` visited =
            go' (S.delete current visited) current
        go visited current =
            let visited' = (if isLower (head current)
                    then S.insert current
                    else id
                    ) visited
            in  sum $ map (go visited') $ neighbours graph current
        go' _       "end"   = 1
        go' visited current | current `S.member` visited = 0
        go' visited current =
            let visited' = (if isLower (head current)
                    then S.insert current
                    else id
                    ) visited
            in  sum $ map (go' visited') $ neighbours graph current

main :: IO ()
main = do
    input <- parse <$> getContents
    print $ solve1 input
    print $ solve2 input
