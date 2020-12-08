import Control.Applicative
import Data.Char
import Data.Maybe
import System.Environment
import System.IO

type Accumulator = Int
type LNumber = Int
data Operation
    = Jmp
    | Acc
    | Nop
    deriving (Show, Eq)
type Argument = Int
type Instruction = (Operation, Argument)
type Line = (LNumber, Instruction)

data Sign
    = Pos
    | Neg
    deriving (Show, Eq)

data State = State
    { pos :: LNumber
    , acc :: Accumulator
    , visited :: [LNumber]
    , prog :: [Line]
    } deriving (Show, Eq)

data Turnout
    = Loop
    | Termination
    deriving (Show, Eq)

-- Parser

newtype Parser a = Parser
    { runParser :: String -> Maybe (String, a)
    }

instance Functor Parser where
    fmap f (Parser p) =
        Parser $ \input -> do
            (rest, a) <- p input
            Just (rest, f a)

instance Applicative Parser where
    pure x = Parser $ \input -> Just (input, x)
    (Parser p1) <*> (Parser p2) =
        Parser $ \input -> do
            (input', f) <- p1 input
            (input'', a) <- p2 input'
            Just (input'', f a)

instance Alternative Parser where
    empty = Parser $ const Nothing
    (Parser p1) <|> (Parser p2) =
        Parser $ \input -> p1 input <|> p2 input

instance Monad Parser where
    (Parser p1) >>= f =
        Parser $ \input -> do
            (input', a) <- p1 input
            runParser (f a) input'

charP :: Char -> Parser Char
charP c = Parser f
    where
        f (x:xs)
            | x == c = Just (xs, c)
            | otherwise = Nothing
        f [] = Nothing

stringP :: String -> Parser String
stringP = sequenceA . map charP

spanP :: (Char -> Bool) -> Parser String
spanP f =
    Parser $ \input ->
        let (token, rest) = span f input
         in Just (rest, token)

spaceP :: Parser Char
spaceP = charP ' '

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) =
    Parser $ \input -> do
        (input', xs) <- p input
        if null xs
            then Nothing
            else Just (input', xs)

intP :: Parser Int
intP = fmap read $ notNull $ spanP isNumber

-- General

opP :: Parser Operation
opP = fmap f $ stringP "acc" <|> stringP "jmp" <|> stringP "nop"
    where
        f "acc" = Acc
        f "jmp" = Jmp
        f "nop" = Nop

signP :: Parser Sign
signP = fmap f $ charP '+' <|> charP '-'
    where
        f '+' = Pos
        f '-' = Neg

argP :: Parser Argument
argP = do
    sign <- signP
    n <- intP
    return (n * f sign)
        where
            f Pos = 1
            f Neg = -1

instructionP :: Parser Instruction
instructionP = do
    op <- opP
    spaceP
    arg <- argP
    return (op, arg)

parseProg :: String -> [Line]
parseProg = zip [0..] . map snd . mapMaybe (runParser instructionP) . lines

newState :: [Line] -> State
newState prog = State 0 0 [] prog

stepProg :: State -> State
stepProg state@(State p a v prog) = state{pos=newPos, acc=newAcc, visited=newVisited}
    where
        (op, arg) =case lookup p prog of
                      Just(x, y) -> (x, y)
                      Nothing -> undefined
        newPos = case op of
                   Jmp -> p + arg
                   _   -> p + 1
        newAcc = case op of
                   Acc -> (seq a) a + arg
                   _   -> a
        newVisited = p:v

runLoop :: State -> [State]
runLoop = iterate stepProg

visitedTwice :: State -> Bool
visitedTwice (State _ _ [] _) = False
visitedTwice s = v `elem` vs
    where
        (v:vs) = visited s

-- One

runOne :: [Line] -> Accumulator
runOne prog = acc $ last states
    where
        states = takeWhile (not . visitedTwice) $ runLoop $ newState prog

-- Two

swapInstruction :: Instruction -> Instruction
swapInstruction (Nop, arg) = (Jmp, arg)
swapInstruction (Jmp, arg) = (Nop, arg)
swapInstruction i = i

adjust :: Eq k => (a -> a) -> k -> [(k,a)] -> [(k,a)]
adjust _ _ []  = []
adjust f k ((pk, pa):ps)
    | k == pk   = (k, f pa) : ps
    | otherwise = (pk, pa)  : adjust f k ps

swapInstructionAt :: [Line] -> Int -> [Line]
swapInstructionAt ls k = adjust swapInstruction k ls

swapAtAllInstructions :: [Line] -> [[Line]]
swapAtAllInstructions ls = map (swapInstructionAt ls) [0 .. (length ls - 1)]

progEnd :: State -> Bool
progEnd (State p _ _ prog) = p >= (length prog)

howTerminates :: State -> Maybe Turnout
howTerminates state
  | progEnd state = Just Termination
  | visitedTwice state = Just Loop
  | otherwise = Nothing

progTurnout :: State -> Turnout
progTurnout prog = head $ mapMaybe howTerminates $ runLoop prog

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil p [] = []
takeUntil p (x:xs)
    | p x = [x]
    | otherwise = takeUntil p xs

progEndAcc :: State -> Maybe Accumulator
progEndAcc state
  | result == Loop = Nothing
  | result == Termination  = Just $ acc $ last $ takeUntil progEnd $ runLoop $ state
      where result = progTurnout state

runTwo :: [Line] -> Accumulator
runTwo prof = head $ mapMaybe (progEndAcc . newState) $ swapAtAllInstructions prof

-- IO

baseIO :: ([Line] -> Accumulator) -> IO ()
baseIO f = do
    args <- getArgs
    content <- readFile $ head args
    putStrLn $ show $ f $ parseProg content

oneIO :: IO ()
oneIO = baseIO runOne

twoIO :: IO ()
twoIO = baseIO runTwo

