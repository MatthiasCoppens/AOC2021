import Data.List

type Block = (Int, Int, Int)

parse :: String -> [Block]
parse = take 14 . go . map words . lines
    where
        go  ( ["inp", "w"]
            : ["mul", "x", "0"]
            : ["add", "x", "z"]
            : ["mod", "x", "26"]
            : ["div", "z", c1]
            : ["add", "x", c2]
            : ["eql", "x", "w"]
            : ["eql", "x", "0"]
            : ["mul", "y", "0"]
            : ["add", "y", "25"]
            : ["mul", "y", "x"]
            : ["add", "y", "1"]
            : ["mul", "z", "y"]
            : ["mul", "y", "0"]
            : ["add", "y", "w"]
            : ["add", "y", c3]
            : ["mul", "y", "x"]
            : ["add", "z", "y"]
            : rest
            ) = (read c1, read c2, read c3) : go rest

main :: IO ()
main = do
    input <- parse <$> getContents
    mapM_ print input
