{-# LANGUAGE TupleSections #-}

import Control.Monad
import Control.Monad.State

type Hex = Char
type Bit = Bool
data Packet = Literal Int | Operator Int [Versioned] deriving Show
type Versioned = (Int, Packet)

hexs2bits :: [Hex] -> [Bit]
hexs2bits ('0' : rest) = False : False : False : False : hexs2bits rest
hexs2bits ('1' : rest) = False : False : False : True  : hexs2bits rest
hexs2bits ('2' : rest) = False : False : True  : False : hexs2bits rest
hexs2bits ('3' : rest) = False : False : True  : True  : hexs2bits rest
hexs2bits ('4' : rest) = False : True  : False : False : hexs2bits rest
hexs2bits ('5' : rest) = False : True  : False : True  : hexs2bits rest
hexs2bits ('6' : rest) = False : True  : True  : False : hexs2bits rest
hexs2bits ('7' : rest) = False : True  : True  : True  : hexs2bits rest
hexs2bits ('8' : rest) = True  : False : False : False : hexs2bits rest
hexs2bits ('9' : rest) = True  : False : False : True  : hexs2bits rest
hexs2bits ('A' : rest) = True  : False : True  : False : hexs2bits rest
hexs2bits ('B' : rest) = True  : False : True  : True  : hexs2bits rest
hexs2bits ('C' : rest) = True  : True  : False : False : hexs2bits rest
hexs2bits ('D' : rest) = True  : True  : False : True  : hexs2bits rest
hexs2bits ('E' : rest) = True  : True  : True  : False : hexs2bits rest
hexs2bits ('F' : rest) = True  : True  : True  : True  : hexs2bits rest
hexs2bits [] = []

bits2dec :: [Bit] -> Int
bits2dec = foldl1 (\x y -> 2 * x + y) . map fromEnum

pop :: State [Bit] Bit
pop = state $ \(b : bs) -> (b, bs)

end :: State [a] Bool
end = null <$> get

readNum :: Int -> State [Bit] Int
readNum k = bits2dec <$> replicateM k pop

literal :: State [Bit] Int
literal = foldl1 (\x y -> 16 * x + y) <$> go
    where
        go = do
            b <- pop
            n <- readNum 4
            if b then (n :) <$> go else return [n]

packet :: State [Bit] Versioned
packet = do
    version <- readNum 3
    typeId  <- readNum 3
    (version, ) <$> case typeId of
        4 -> Literal <$> literal
        _ -> do
            lengthTypeId <- pop
            Operator typeId <$> (if lengthTypeId
                then do
                    nSubPackets <- readNum 11
                    replicateM nSubPackets packet
                else do
                    nBits <- readNum 15
                    evalState packets <$> (state $ splitAt nBits)
                )

packets :: State [Bit] [Versioned]
packets = do
    b <- end
    if b then return [] else (:) <$> packet <*> packets

versions :: Versioned -> [Int]
versions (n, packet) = n : case packet of
    Operator _ children -> concatMap versions children
    _                   -> []

eval :: Versioned -> Int
eval (_, packet) = go packet
    where
        go (Literal n) = n
        go (Operator k versioneds) = case (k, map eval versioneds) of
            (0, ns)       -> sum ns
            (1, ns)       -> product ns
            (2, ns)       -> minimum ns
            (3, ns)       -> maximum ns
            (5, (n:n':_)) -> fromEnum $ n >  n'
            (6, (n:n':_)) -> fromEnum $ n <  n'
            (7, (n:n':_)) -> fromEnum $ n == n'

parse :: String -> Versioned
parse = evalState packet . hexs2bits . head . lines

solve1 :: Versioned -> Int
solve1 = sum . versions

solve2 :: Versioned -> Int
solve2 = eval

main :: IO ()
main = do
    input <- parse <$> getContents
    print $ solve1 input
    print $ solve2 input
