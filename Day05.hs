{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Text as T
import Data.Function ((&))
import Data.List (isSubsequenceOf, elemIndex, elemIndices)
import Data.Maybe (fromJust)

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


part1 :: ([[Int]], [[Int]]) -> Int
part1 (antiRules, pageEntries) = sum $ pageEntries
    & filter (areWellOrdered antiRules)
    & map middlePage 

swapElements :: [Int] -> [Int] -> [Int]
swapElements (a : b : []) list = (take idx list) 
    ++ [b] 
    ++ (take (idx' - idx - 1) . drop (idx + 1) $ list) 
    ++ [a] 
    ++ (drop (idx' + 1) list)
  where
    idx = fromJust $ elemIndex a list 
    idx' = fromJust $ elemIndex b list
swapElements _ list = list

part2 :: ([[Int]], [[Int]]) -> Int
part2 (antiRules, pageEntries) = sum $ pageEntries
    & filter (not . areWellOrdered antiRules)
    & map fixOrdering
    & map middlePage
  where
    rulesViolated :: [Int] -> [[Int]]
    rulesViolated pages = antiRules
        & map (`isSubsequenceOf` pages)
        & elemIndices True
        & map (antiRules !!)

    fixOrdering :: [Int] -> [Int]
    fixOrdering pages = go pages False

    go :: [Int] -> Bool -> [Int]
    go pages True = pages
    go pages False = let violated = rulesViolated pages
                         pages' = swapElements (violated !! 0) pages
                     in go pages' (areWellOrdered antiRules pages')

main :: IO ()
main = do
    content <- readFile "input5.txt"
    
    let parsed = parse . T.pack $ content
    
    let resultPart1 = part1 parsed
    let resultPart2 = part2 parsed
    
    putStrLn $ "Part 1: " ++ show resultPart1
    putStrLn $ "Part 2: " ++ show resultPart2
