{-# LANGUAGE LambdaCase #-}
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.Maybe (catMaybes)
import Control.Monad (void, when)

data Op = 
  Mul Int Int
  | Do
  | Dont
  deriving (Show)

parser :: Parsec Void String [Op]
parser = catMaybes <$> many (do
    end <- atEnd
    when end $ fail "end of input"
    _ <- manyTill anySingle (lookAhead (void (doParser <|> dontParser <|> mulParser) <|> eof)) 
    optional (doParser <|> dontParser <|> mulParser))
  where
    mulParser :: Parsec Void String Op
    mulParser = try $ do
        _ <- string "mul("
        a <- read <$> many digitChar
        _ <- char ','
        b <- read <$> many digitChar
        _ <- char ')' 
        pure (Mul a b)
    doParser :: Parsec Void String Op
    doParser = try $ string "do()" >> pure Do
    dontParser :: Parsec Void String Op
    dontParser = try $ string "don't()" >> pure Dont
    
part1 :: Either a [Op] -> Int
part1 = \case
    Left _ -> 0
    Right ops -> sum $ map go ops
  where
    go :: Op -> Int
    go = \case
        (Mul a b) -> a * b
        _ -> 0
part2 :: Either a [Op] -> Int
part2 = \case
    Left _ -> 0
    Right ops -> sum $ go True ops
  where
    go :: Bool -> [Op] -> [Int]
    go True ((Mul a b) : ops) = (a * b) : (go True ops)
    go _ (Do : ops) = go True ops
    go _ (Dont : ops) = go False ops
    go False (_ : ops) = go False ops
    go _ [] = []

main :: IO ()
main = do
    content <- readFile "input3.txt"
    let parsed = parse parser "" content
    let resultPart1 = part1 parsed
    let resultPart2 = part2 parsed
    print parsed
    putStrLn $ "Part 1: " ++ show resultPart1
    putStrLn $ "Part 2: " ++ show resultPart2
