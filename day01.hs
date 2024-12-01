{-# LANGUAGE OverloadedStrings #-}
import Data.List (sort)
import qualified Data.Text as T

solve :: [Int] -> [Int] -> (Int, Int)
solve xs ys = 
    let
      part1 = sum $ zipWith (\x y -> abs (x - y)) xs ys
      part2 = foldl (\acc x -> acc + (x * (length $ filter (== x) ys))) 0 xs
    in (part1, part2)


parse :: String -> ([Int], [Int])
parse = go ([], []) 
    . words 
    . T.unpack 
    . T.replace "   " " " 
    . T.replace "\n" " " 
    . T.pack
  where
    go :: ([Int], [Int]) -> [String] -> ([Int], [Int])
    go res [] = (sort . fst $ res, sort . snd $ res)
    go res (x1 : x2 : xs) = go ((read x1) : (fst res), (read x2) : (snd  res)) xs

main :: IO ()
main = do
    content <- readFile "input1.txt"
    let result = (uncurry solve) . parse $ content
    print result

