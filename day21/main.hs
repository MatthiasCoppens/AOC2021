import Data.Char
import Text.ParserCombinators.ReadP
import qualified Data.Set as S

type Cuboid = ((Int, Int), (Int, Int), (Int, Int))

num :: ReadP Int
num = (option id $ negate <$ char '-') <*> (read <$> munch1 isDigit)

onOff :: ReadP Bool
onOff = (True <$ string "on") +++ (False <$ string "off")

range :: ReadP (Int, Int)
range = (,) <$> num <*> (string ".." *> (succ <$> num))

cuboid :: ReadP Cuboid
cuboid = (,,) <$> (string " x=" *> range) <*> (string ",y=" *> range)
    <*> (string ",z=" *> range)

step :: ReadP (Bool, Cuboid)
step = (,) <$> onOff <*> cuboid

steps :: ReadP [(Bool, Cuboid)]
steps = sepBy step $ char '\n'


realCuboid :: Cuboid -> Bool
realCuboid ((x0, x1), (y0, y1), (z0, z1)) = (x0 < x1) && (y0 < y1) && (z0 < z1)

remove1 :: Cuboid -> Cuboid -> [Cuboid]
remove1 ((x1, x2), (y1, y2), (z1, z2)) ((x0, x3), (y0, y3), (z0, z3))
    | not $ realCuboid ((x1, x2), (y1, y2), (z1, z2)) = 
        [((x0, x3), (y0, y3), (z0, z3))]
    | x1 < x0   =
        remove1 ((x0, x2), (y1, y2), (z1, z2)) ((x0, x3), (y0, y3), (z0, z3))
    | y1 < y0   =
        remove1 ((x1, x2), (y0, y2), (z1, z2)) ((x0, x3), (y0, y3), (z0, z3))
    | z1 < z0   =
        remove1 ((x1, x2), (y1, y2), (z0, z2)) ((x0, x3), (y0, y3), (z0, z3))
    | x2 > x3   =
        remove1 ((x1, x3), (y1, y2), (z1, z2)) ((x0, x3), (y0, y3), (z0, z3))
    | y2 > y3   =
        remove1 ((x1, x2), (y1, y3), (z1, z2)) ((x0, x3), (y0, y3), (z0, z3))
    | z2 > z3   =
        remove1 ((x1, x2), (y1, y2), (z1, z3)) ((x0, x3), (y0, y3), (z0, z3))
    | otherwise =
        filter realCuboid
        [ ((x0, x1), (y0, y1), (z0, z1))
        , ((x0, x1), (y0, y1), (z1, z2))
        , ((x0, x1), (y0, y1), (z2, z3))
        , ((x0, x1), (y1, y2), (z0, z1))
        , ((x0, x1), (y1, y2), (z1, z2))
        , ((x0, x1), (y1, y2), (z2, z3))
        , ((x0, x1), (y2, y3), (z0, z1))
        , ((x0, x1), (y2, y3), (z1, z2))
        , ((x0, x1), (y2, y3), (z2, z3))
        , ((x1, x2), (y0, y1), (z0, z1))
        , ((x1, x2), (y0, y1), (z1, z2))
        , ((x1, x2), (y0, y1), (z2, z3))
        , ((x1, x2), (y1, y2), (z0, z1))
        , ((x1, x2), (y1, y2), (z2, z3))
        , ((x1, x2), (y2, y3), (z0, z1))
        , ((x1, x2), (y2, y3), (z1, z2))
        , ((x1, x2), (y2, y3), (z2, z3))
        , ((x2, x3), (y0, y1), (z0, z1))
        , ((x2, x3), (y0, y1), (z1, z2))
        , ((x2, x3), (y0, y1), (z2, z3))
        , ((x2, x3), (y1, y2), (z0, z1))
        , ((x2, x3), (y1, y2), (z1, z2))
        , ((x2, x3), (y1, y2), (z2, z3))
        , ((x2, x3), (y2, y3), (z0, z1))
        , ((x2, x3), (y2, y3), (z1, z2))
        , ((x2, x3), (y2, y3), (z2, z3))
        ]
    where

remove :: Cuboid -> [Cuboid] -> [Cuboid]
remove = concatMap . remove1

add :: Cuboid -> [Cuboid] -> [Cuboid]
add cuboid = (cuboid :) . remove cuboid

merge :: [(Bool, Cuboid)] -> [Cuboid]
merge = foldl go []
    where
        go ans (b, cuboid) = case b of
            False -> remove cuboid ans
            True  -> add    cuboid ans

size :: Cuboid -> Int
size ((x0, x1), (y0, y1), (z0, z1)) = (x1 - x0) * (y1 - y0) * (z1 - z0)

region :: [Cuboid] -> [Cuboid]
region = filter realCuboid . map (\((x0, x1), (y0, y1), (z0, z1)) -> ((max (-50) x0, min 51 x1), (max (-50) y0, min 51 y1), (max (-50) z0, min 51 z1)))


parse :: String -> [(Bool, Cuboid)]
parse = fst . last . readP_to_S steps

main :: IO ()
main = do
    input <- (merge . parse) <$> getContents
    print $ sum $ map size $ region input
    print $ sum $ map size $        input
