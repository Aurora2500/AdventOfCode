import Control.Applicative
import Control.Monad
import Data.Char
import Data.Tuple

-- Parser implementation

newtype Parser a = Parser {runParser :: String -> Maybe (String, a) }

instance Functor Parser where
    fmap f (Parser p) = Parser $ \input -> do
        (rest, x) <- p input
        pure (rest, f x)

instance Applicative Parser where
    pure x = Parser $ \input -> Just(input, x)
    (Parser p1) <*> (Parser p2) = Parser $ \input -> do
        (y1, f) <- p1 input
        (y2, x) <- p2 y1
        pure (y2, f x)

instance Alternative Parser where
    empty = Parser $ const Nothing
    (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input

instance Monad Parser where
    (Parser p) >>= f =
        Parser $ \input -> do
            (y1, x1) <- p input
            runParser (f x1) y1

charP :: Char -> Parser Char
charP c = Parser $ \input -> f input
    where
        f (x:xs)
            | x == c = Just(xs, x)
            | otherwise = Nothing
        f [] = Nothing

stringP :: String -> Parser String
stringP = sequence . fmap charP

spanP :: (Char -> Bool) -> Parser String
spanP pred = Parser $ \input -> Just $ swap $ span pred input

nonEmptyP :: Parser String -> Parser String
nonEmptyP (Parser p) = Parser $ \input -> do
    (rest, x) <- p input
    guard (not $ null $ x)
    pure (rest, x)

intP :: Parser Int
intP = fmap read $ nonEmptyP $ spanP isDigit

wsP :: Parser String
wsP = spanP isSpace

-- General

parseDraws :: Parser [Int]
parseDraws = do
    charP '['
    xs <- many $ (intP <* (optional $ charP ','))
    stringP "]\n\n"
    pure xs

type Board = [[Int]]

parseBoard :: Parser Board
parseBoard = do
    row <- many $ 

parseBoards :: Parser [Board]
parseBoards = do


sol1 :: String -> String
sol1 input = undefined

main :: IO ()
main = do
    putStrLn "Hello, world!"

