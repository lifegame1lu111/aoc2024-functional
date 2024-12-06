{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import Data.Function ((&))
import Data.List (isSubsequenceOf, sortBy, partition)

parse :: T.Text -> ([[Int]], [[Int]])
parse content = (antiRules, pageEntries)
  where
    parts :: [T.Text]
    parts = T.splitOn "\n\n" content

    antiRules :: [[Int]]
    antiRules = parts !! 0
        & T.lines
        & map ((reverse . read . go) :: T.Text -> [Int])

    go :: T.Text -> String
    go rule = T.unpack . T.intercalate (T.replace "|" "," rule) $ ["[", "]"]
        
    pageEntries :: [[Int]]
    pageEntries = parts !! 1
        & T.lines
        & map ((read . go') :: T.Text -> [Int])
    
    go' :: T.Text -> String
    go' pages = T.unpack . T.intercalate pages $ ["[", "]"]

middlePage :: [Int] -> Int
middlePage pages = pages !! (length pages `quot` 2)

areWellOrdered :: [[Int]] -> [Int] -> Bool
areWellOrdered antiRules pages = not . or $ antiRules
    & map (`isSubsequenceOf` pages)

part1 :: [[Int]] -> Int
part1 pageEntries = sum $ pageEntries
    & map middlePage 

part2 :: ([[Int]], [[Int]]) -> Int
part2 (antiRules, pageEntries) = sum $ pageEntries
    & map (sortBy fixOrdering)
    & map middlePage
  where
    fixOrdering :: Int -> Int -> Ordering
    fixOrdering a b = if [a, b] `elem` antiRules then GT else LT

main :: IO ()
main = do
    content <- readFile "input5.txt"
    
    let (antiRules, pageEntries) = parse . T.pack $ content

    let (ordered, unordered) = partition (areWellOrdered antiRules) pageEntries
    
    let resultPart1 = part1 ordered
    let resultPart2 = part2 (antiRules, unordered)
    
    putStrLn $ "Part 1: " ++ show resultPart1
    putStrLn $ "Part 2: " ++ show resultPart2
