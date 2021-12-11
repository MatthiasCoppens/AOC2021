import Data.Either
import Data.List

wrongPar :: String -> Either Char String
wrongPar = go ""
    where
        go stack ('(' : cs) = go (')' : stack) cs
        go stack ('[' : cs) = go (']' : stack) cs
        go stack ('{' : cs) = go ('}' : stack) cs
        go stack ('<' : cs) = go ('>' : stack) cs
        go (c' : stack) (c : cs)
            | c == c'   = go stack cs
            | otherwise = Left c
        go stack [] = Right stack

solve1 :: [Char] -> Word
solve1 = sum . map score
    where
        score ')' = 3
        score ']' = 57
        score '}' = 1197
        score '>' = 25137

solve2 :: [String] -> Word
solve2 = mid . sort . map (score 0) . filter (not . null)
    where
        mid [x] = x
        mid xs  = mid $ tail $ init xs
        score s "" = s
        score s (')' : rest) = score (5 * s + 1) rest
        score s (']' : rest) = score (5 * s + 2) rest
        score s ('}' : rest) = score (5 * s + 3) rest
        score s ('>' : rest) = score (5 * s + 4) rest

main :: IO ()
main = do
    (input1, input2) <- partitionEithers . map wrongPar . lines <$> getContents
    print $ solve1 input1
    print $ solve2 input2
