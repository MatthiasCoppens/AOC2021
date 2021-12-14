import Text.ParserCombinators.ReadP hiding (count)
import qualified Data.Map.Strict as M

type Freq = M.Map Char Word
type Pair = (Char, Char)

freqs :: M.Map Pair Char -> [M.Map Pair Freq]
freqs mid = iterate f $ M.map (const M.empty) mid
    where
        f freqMaps = M.mapWithKey (
            \(x, y) freqMap -> case mid M.!? (x, y) of
                Nothing -> freqMap
                Just z  -> M.insertWith (+) z 1 $ M.unionWith (+)
                    (freqMaps M.! (x, z)) (freqMaps M.! (z, y))
            ) freqMaps

count :: M.Map Pair Freq -> String -> Freq
count freqMaps = go
    where
        go [c] = M.singleton c 1
        go (c : cs@(c' : _)) = M.unionsWith (+)
            [go [c], go cs, freqMaps M.! (c, c')]

parse :: String -> (String, M.Map Pair Char)
parse = fst . last . readP_to_S parser
    where
        pair = M.singleton <$> ((,) <$> get <*> get) <*> (string " -> " *> get)
        pairs = M.unions <$> sepBy pair (char '\n')
        parser = (,) <$> many get <*> (string "\n\n" *> pairs)

solve :: M.Map Pair Freq -> String -> Word
solve freq = ((-) <$> maximum <*> minimum) . count freq

main :: IO ()
main = do
    (start, m) <- parse <$> getContents
    let fs = freqs m
    print $ solve (fs !! 10) start
    print $ solve (fs !! 40) start
