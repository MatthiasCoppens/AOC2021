import qualified Data.Map.Strict as M
import Data.List

freqs :: (Foldable t, Ord a) => t a -> M.Map a Word
freqs = foldr (\x -> M.insertWith (+) x 1) M.empty

invert :: (Ord a, Ord b) => M.Map a b -> M.Map b a
invert = M.foldrWithKey (\k v -> M.insertWith max v k) M.empty

mostCommon :: (Foldable t, Ord a) => t a -> a
mostCommon xs = snd $ M.findMax $ invert $ freqs xs

bin2dec :: [Bool] -> Int
bin2dec [] = 0
bin2dec bs = 2 * bin2dec (init bs) + fromEnum (last bs)

mostCommons :: Ord a => Int -> [[a]] -> [[a]]
mostCommons i bss =
    let b = mostCommon $ map (!! i) bss
    in  filter ((== b) . (!! i)) bss

leastCommons :: Ord a => Int -> [[a]] -> [[a]]
leastCommons i bss =
    let b = mostCommon $ map (!! i) bss
    in  filter ((/= b) . (!! i)) bss

condense :: Ord a => (Int -> [[a]] -> [[a]]) -> [[a]] -> [a]
condense f bss@(bs : _) = go bss $ cycle $ zipWith const [0 ..] bs
    where
        go [bs] _        = bs
        go bss  (i : is) = go (f i bss) is


parse :: String -> [[Bool]]
parse = (map . map) (== '1') . lines

solve1 :: [[Bool]] -> Int
solve1 = f . map mostCommon . transpose
    where
        f bs =
            let gamma   = bin2dec           bs
                epsilon = bin2dec $ not <$> bs
            in  gamma * epsilon

solve2 :: [[Bool]] -> Int
solve2 bss =
    let oxygen = bin2dec $ condense mostCommons  bss
        carbon = bin2dec $ condense leastCommons bss
    in  oxygen * carbon

main :: IO ()
main = do
    input <- parse <$> getContents
    print $ solve1 input
    print $ solve2 input
