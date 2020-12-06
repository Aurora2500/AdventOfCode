import Data.Char
import Data.List
import qualified Data.Map as M
import Data.Maybe
import System.Environment
import System.IO

type Key = String

type Pass = M.Map String String
type Field = (Key, String)

type Validator = Pass -> Bool

separatePass :: String -> [String]
separatePass = map unwords . filter (/= [[]]) . groupBy (\ x y -> and [x /= "", y /= ""]) . lines

parsePass :: String -> Pass
parsePass = M.fromList . map parseField  . words

parseField :: String -> Field
parseField str = (k, v)
    where split = break (== ':') str
          k = fst split
          v = tail $ snd split

countValidPasses :: Validator -> [Pass] -> Int
countValidPasses validator = length . filter id . map validator

-- One

validatePassOne :: Validator
validatePassOne = and . ( sequenceA $ map M.member reqKeys )
    where
        reqKeys = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

-- Two

validatePassTwo :: Validator
validatePassTwo = and . sequenceA [vByr, vIyr, vEyr, vHgt, vHcl, vEcl, vPid]

between :: Int -> Int -> Int -> Bool
between low high val = (low <= val) && (high >= val)

validField :: (String -> Bool) -> Key -> Validator
validField val key = fromMaybe False . fmap  val . res
    where res = M.lookup key

betweenVal :: Int -> Int -> String -> Validator
betweenVal low high k = validField val k
    where
        val x = between low high (read x)

vByr :: Validator
vByr = betweenVal 1920 2002 "byr"

vIyr :: Validator
vIyr = betweenVal 2010 2020 "iyr"

vEyr :: Validator
vEyr = betweenVal 2020 2030 "eyr"

vHgt :: Validator
vHgt = validField val "hgt"
    where
        val x
            | isSuffixOf "cm" x = between 150 193 $ num x
            | isSuffixOf "in" x = between 59 76 $ num x
            | otherwise = False
        num n =  read $ filter (isNumber) n

vHcl :: Validator
vHcl = validField val "hcl"
    where
        val ('#':xs)
          | length xs == 6 = and $ fmap isHex xs
        val _ = False
        isHex x = elem x "abcdef1234567890"

vEcl :: Validator
vEcl = validField val "ecl"
    where
        val x = elem x ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

vPid :: Validator
vPid = validField val "pid"
    where
        val xs
            | length xs == 9 = and $ fmap isNumber xs
            | otherwise = False

-- IO

baseIO :: Validator -> IO ()
baseIO validator= do
    args <- getArgs
    content <- readFile $ head args
    putStrLn $ show $ countValidPasses validator $ map parsePass $ separatePass content

oneIO :: IO ()
oneIO = baseIO validatePassOne

twoIO :: IO ()
twoIO = baseIO validatePassTwo

