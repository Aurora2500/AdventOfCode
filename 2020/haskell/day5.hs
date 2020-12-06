import System.Environment
import System.IO

-- Types

type Pass = (Int, Int)
type ID = Int

getID :: Pass -> ID
getID (r,c) = r * 8 + c

fromID :: ID -> Pass
fromID id = (r,c)
    where
        r = div id  8
        c = mod id 8

calcPos :: String -> Int
calcPos str = sum $ zipWith (*) pos bin
    where
        pos = reverse $ take (length str) $ iterate (2*) 1
        bin = [ if (c `elem` "BR") then 1 else 0 | c <- str ]

parsePass :: String -> Pass
parsePass str = (r,c)
    where
        (rs,cs) = splitAt 7 str
        r = calcPos rs
        c = calcPos cs

-- One

solOne :: [String] -> ID
solOne = maximum . map (getID . parsePass)

-- Two

validID :: [ID] -> ID -> Bool
validID ids id = (id - 1 `elem` ids) && (id + 1 `elem` ids)

findValidID :: [ID] -> [ID]
findValidID ids = [id | id <- [0..888], not $ elem id ids, validID ids id]

solTwo :: [String]  -> ID
solTwo = head . findValidID . map (getID . parsePass)

-- IO

baseIO :: ([String] -> Int) -> IO ()
baseIO sol = do
    args <- getArgs
    content <- readFile $ head args
    putStrLn $ show $ sol $ lines content

oneIO :: IO ()
oneIO = baseIO solOne

twoIO :: IO ()
twoIO = baseIO solTwo

