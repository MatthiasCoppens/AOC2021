import Data.Either
import qualified Data.Set as S

type Coord = (Int, Int)

size :: [[a]] -> Coord
size = (,) <$> length . head <*> length

cucumbers :: [[Char]] -> (S.Set Coord, S.Set Coord)
cucumbers grid =
    let (e, s) = partitionEithers
            [ (if c == '>' then Left else Right) (x, y)
            | (y, l) <- zip [0 ..] grid
            , (x, c) <- zip [0 ..] l
            , c /= '.'
            ]
    in  (S.fromList e, S.fromList s)

stepEast, stepSouth :: Int -> (S.Set Coord, S.Set Coord) -> S.Set Coord
stepEast xMax (east, south) =
    let allCucumbers = S.union east south
        toEast (x, y) =
            let c' = ((x + 1) `mod` xMax, y)
            in  if c' `S.member` allCucumbers then (x, y) else c'
    in  S.map toEast east
stepSouth yMax (east, south) =
    let allCucumbers = S.union east south
        toSouth (x, y) =
            let c' = (x, (y + 1) `mod` yMax)
            in  if c' `S.member` allCucumbers then (x, y) else c'
    in  S.map toSouth south

step :: Coord -> (S.Set Coord, S.Set Coord) -> (S.Set Coord, S.Set Coord)
step (xMax, yMax) (east, south) =
    let east' = stepEast xMax (east, south)
        south' = stepSouth yMax (east', south)
    in  (east', south')

parse :: String -> (Coord, (S.Set Coord, S.Set Coord))
parse = ((,) <$> size <*> cucumbers) . lines

solve :: Coord -> (S.Set Coord, S.Set Coord) -> Int
solve s = go . iterate (step s)
    where
        go (x1 : x2 : _) | x1 == x2 = 1
        go (_ : xs) = succ $ go xs

main :: IO ()
main = do
    (s, sets) <- parse <$> getContents
    print $ solve s sets
