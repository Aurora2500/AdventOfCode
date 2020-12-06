import Data.List
import Data.Set hiding (map, filter, foldl)

import System.Environment
import System.IO

type Question = Set Char
type Group = [Question]

separateGroups :: String -> [Group]
separateGroups = map (map fromList) . filter (/= [[]]) . groupBy (\ x y -> and [x /= "", y /= ""]) . lines

sumBy :: (Group -> Int) -> String -> Int
sumBy c = sum . map c . separateGroups

-- One

countAny :: Group -> Int
countAny = size . unions

sumAny :: String -> Int
sumAny = sumBy countAny

-- Two

countAll :: Group -> Int
countAll (q:qs) = size $ foldl intersection q qs

sumAll :: String -> Int
sumAll = sumBy countAll

-- IO

baseIO :: (String -> Int) -> IO ()
baseIO f = do
    args <- getArgs
    content <- readFile $ head args
    putStrLn $ show $ f content

oneIO :: IO ()
oneIO = baseIO sumAny

twoIO :: IO ()
twoIO = baseIO sumAll

