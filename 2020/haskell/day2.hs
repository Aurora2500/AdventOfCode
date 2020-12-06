import Control.Arrow
import Data.Char
import System.Environment
import System.IO

-- General

type Rule = (Int, Int, Char)
type Password = String
type Entry = (Rule, Password)
type Validator = Entry -> Bool

parseRule :: String -> Rule
parseRule s = (left, right, char)
    where
        (times, char) = break (== ' ') >>> second (tail >>> head) $ s
        (left, right) = break (not . isNumber) >>> read *** (tail >>> read) $ times

parseEntry :: String -> Entry
parseEntry s = (rule, password)
    where
        (rule, password) = break (== ':') >>> parseRule *** drop 2 $ s


validateLine :: Validator -> String -> Bool
validateLine validator = parseEntry >>> validator

countValidEntries :: Validator -> [String] -> Int
countValidEntries validator = map (validateLine validator) >>> filter id >>> length

-- One

validateOne :: Validator
validateOne ((least, most, char), password) = (appearance >= least) && (appearance <= most)
    where
        appearance = filter (== char) >>> length $ password

-- Two

validateTwo :: Validator
validateTwo ((left, right, char), password) = (chleft == char) /= (chright == char)
    where
        chleft = password !! (left - 1)
        chright = password !! (right - 1)

-- IO

validIO :: Validator -> IO()
validIO validator = do
    args <- getArgs
    content <- readFile $ head args
    putStrLn $ show $ countValidEntries validator $ lines content

oneIO :: IO()
oneIO = validIO validateOne

twoIO :: IO()
twoIO = validIO validateTwo

