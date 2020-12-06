import System.Environment
import System.IO

-- Types

type Map   = [[Char]]
type Path  = [Char]
type Slope = (Int, Int) -- Down - Right

-- General

extendMap :: Map -> Map
extendMap = map cycle

slide :: Slope -> Map -> Path
slide sl  [] = []
slide sl ms = head m : slide sl (map (drop lft) (drop dwn ms))
    where m = head ms
          (dwn, lft) = sl

hitTrees :: Path -> Int
hitTrees = length . filter (=='#')

-- One

oneSol :: Map -> Int
oneSol = hitTrees . slide (1, 3) . extendMap

-- Two

twoSol :: Map -> Int
twoSol = product . map hitTrees . sequenceA (map slide slopes) . extendMap
    where
        slopes = [(1,1), (1,3), (1,5), (1,7), (2,1)]

-- IO

baseIO ::(Map -> Int) -> IO ()
baseIO f = do
    args <- getArgs
    content <- readFile $ head args
    putStrLn $ show $ f $ lines content

oneIO :: IO ()
oneIO = baseIO oneSol

twoIO :: IO ()
twoIO = baseIO twoSol

