{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
import Data.Maybe (isNothing)

data Grid = Grid
    { elements :: [(Char, Pos)]
    , rawData :: [String]
    , dimensions :: (Int, Int)
    } deriving (Show)

data Direction = 
    N
    | NE
    | NW
    | S
    | SE
    | SW
    | E 
    | W
    deriving (Show)

data Pos = Pos 
    { x :: Int
    , y :: Int
    , nearWalls :: Maybe Direction
    } deriving (Show)

labelGrid :: Int -> [String] -> (Int, Int) -> Grid
labelGrid distance elems size = Grid
    { elements = [ (elems !! x !! y, Pos x y (near (x, y))) | x <- [0..xMax], y <- [0..yMax]]
    , rawData = elems
    , dimensions = size
    }
  where
    xMax = fst size - 1
    yMax = snd size - 1

    isNear :: Int -> Int -> Bool
    isNear idx wallIdx = abs (wallIdx - idx) <= distance  

    near :: (Int, Int) -> Maybe Direction
    near (x, y)
        | x `isNear` 0 && y `isNear` 0       = Just NW
        | x `isNear` xMax && y `isNear` 0    = Just NE
        | x `isNear` 0 && y `isNear` yMax    = Just SW
        | x `isNear` xMax && y `isNear` yMax = Just SE
        | y `isNear` 0                       = Just N
        | y `isNear` yMax                    = Just S
        | x `isNear` 0                       = Just W
        | x `isNear` xMax                    = Just E
        | otherwise                          = Nothing

getGridDims :: [String] -> (Int, Int)
getGridDims elements@(line : _) = (length line, length elements)
getGridDims _ = (0, 0)

part1 :: Grid -> Int
part1 Grid { elements, rawData } = sum $ map
    (countXmas . snd)
    (filter (\(c, _) -> c == 'X') elements)
  where
    deltas = [0..3]
    deltas' = [(0, 0), (1, 1), (2, 2), (3, 3)]

    search :: (Int, Int) -> [Direction] -> Int
    search _ [] = 0
    search (x, y) (d : ds) = search (x, y) ds + (fromEnum . ("XMAS" ==) $ go (x, y) d)

    go :: (Int, Int) -> Direction -> String
    go (x, y) d = case d of
        N -> [(rawData !! x) !! (y - dy) | dy <- deltas]
        S -> [(rawData !! x) !! (y + dy) | dy <- deltas]
        E -> [(rawData !! (x + dx) !! y) | dx <- deltas]
        W -> [(rawData !! (x - dx) !! y) | dx <- deltas]
        NE -> [(rawData !! (x + dx)) !! (y - dy) | (dx, dy) <- deltas']
        NW -> [(rawData !! (x - dx)) !! (y - dy) | (dx, dy) <- deltas']
        SE -> [(rawData !! (x + dx)) !! (y + dy) | (dx, dy) <- deltas']
        SW -> [(rawData !! (x - dx)) !! (y + dy) | (dx, dy) <- deltas']

    countXmas :: Pos -> Int
    countXmas (Pos x y direction) = search (x, y) $ case direction of
        Just d -> case d of
            N -> [E, W, SE, SW, S]
            S -> [E, W, NE, NW, N]
            E -> [W, NW, SW, N, S]
            W -> [E, NE, SE, N, S]
            NE -> [S, W, SW]
            NW -> [S, E, SE]
            SE -> [N, W, NW]
            SW -> [N, E, NE]
        Nothing -> [N, S, E, W, NE, NW, SE, SW]

part2 :: Grid -> Int
part2 Grid { elements, rawData } = sum $ map 
    (countXmas . snd)
    (filter (\(c, (Pos _ _ d)) -> c == 'A' && isNothing d) elements)
  where
    arrangements = [ "SSAMM"
                   , "MMASS"
                   , "MSAMS"
                   , "SMASM"
                   ]

    deltas = [(-1, 1)
             , (1, 1)
             , (0, 0)
             , (-1, -1)
             , (1, -1)
             ]
    
    getCross :: (Int, Int) -> String
    getCross (x, y) = [(rawData !! (x + dx)) !! (y + dy) | (dx, dy) <- deltas]

    countXmas :: Pos -> Int
    countXmas (Pos x y _) = 
        let cross = getCross (x, y)
        in fromEnum . any (== cross) $ arrangements

main :: IO ()
main = do
    content <- readFile "input4.txt"

    let grid = lines content
    let gridDims = getGridDims grid 
    let grid' = labelGrid 2 grid gridDims

    let resultPart1 = part1 grid'

    let grid'' = labelGrid 0 grid gridDims

    let resultPart2 = part2 grid''

    putStrLn $ "Part 1: " ++ show resultPart1
    putStrLn $ "Part 2: " ++ show resultPart2
