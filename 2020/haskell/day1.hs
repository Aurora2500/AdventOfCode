import System.Environment
import System.IO

-- General

parseLines :: String -> [Int]
parseLines = map read . lines

-- One

twoSumsTo :: Int -> [Int] -> [(Int, Int)]
twoSumsTo goal objective = [(x, y) | x <- objective, y <- objective, x + y == goal]

partOne :: [Int] -> Int
partOne = uncurry (*) . head . twoSumsTo 2020

-- Two

threeSumsTo :: Int -> [Int] -> [(Int, Int, Int)]
threeSumsTo goal from = [(x, y, z) | x <- from, y <- from, z <- from, x + y + z == goal]

partTwo :: [Int] -> Int
partTwo = (\(x, y, z) -> x*y*z) . head .threeSumsTo 2020

-- IO

baseIO :: ([Int] -> Int) -> IO ()
baseIO f = do
    args <- getArgs
    content <- readFile $ head args
    putStrLn $ show $ f $ parseLines content

oneIO :: IO ()
oneIO = baseIO partOne

twoIO :: IO ()
twoIO = baseIO partTwo

