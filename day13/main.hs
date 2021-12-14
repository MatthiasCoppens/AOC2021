import Text.ParserCombinators.ReadP
import qualified Data.Set as S

data Fold = FoldX Int | FoldY Int deriving Show


num :: ReadP Int
num = fmap read $ many1 $ foldl1 (+++) $ map char ['0' .. '9']

tuple :: ReadP (Int, Int)
tuple = (,) <$> num <*> (char ',' *> num)

set :: ReadP (S.Set (Int, Int))
set = fmap S.fromList $ sepBy tuple $ char '\n'

fold :: ReadP Fold
fold = (string "fold along " *> ((FoldX <$ char 'x') +++ (FoldY <$ char 'y')))
    <*> (char '=' *> num)

folds :: ReadP [Fold]
folds = sepBy fold $ char '\n'

parser :: ReadP (S.Set (Int, Int), [Fold])
parser = (,) <$> set <*> (string "\n\n" *> folds)


makeFold :: Fold -> S.Set (Int, Int) -> S.Set (Int, Int)
makeFold (FoldX x0) = S.map $ \(x, y) -> (min x (2 * x0 - x), y)
makeFold (FoldY y0) = S.map $ \(x, y) -> (x, min y (2 * y0 - y))

parse :: String -> (S.Set (Int, Int), [Fold])
parse = fst . last . readP_to_S parser

solve1 :: S.Set (Int, Int) -> [Fold] -> Int
solve1 grid (fold : _) = S.size $ makeFold fold grid

main :: IO ()
main = do
    (grid, folds) <- parse <$> getContents
    print $ solve1 grid folds
    mapM_ putStrLn
        [   [ if (x, y) `S.member` grid' then '#' else ' '
            | x <- [0 .. 50]
            ]
        | let grid' = foldl (flip makeFold) grid folds
        , y <- [0 .. 5]
        ]
