-- import Control.Monad
import Text.Read
import Data.Maybe
import Text.Printf
import Control.Monad
import Distribution.Compat.CharParsing (CharParsing(char))
main = do
    putStrLn "Hello to my little sticks game\nEnter the number of sticks per row"
    startSticks <- getInts "Number of sticks should be integer but got \"%s\". Try again."
    putStrLn $ "Thanks! You've entered " ++ show startSticks ++ " sticks!"
    handleInput (calculateInitState startSticks) 0

calculateInitState = map (`replicate` True)

printRow maxlen row = do
    let rowLength = length row
        spaceLength = (maxlen - rowLength) `div` 2
        charsInbetween = map (\x -> if x then '|' else 't') row
    putStrLn $ replicate spaceLength ' ' ++ charsInbetween ++ replicate spaceLength ' '

printState state = do
    mapM_ (printRow (maximum $ map length state)) state

handleInput state player
    | not $ all and state = do putStrLn $ "Player " ++ show (1-player) ++ " won"
    | otherwise  = do
    printState state
    (line, position, len) <- getMove state player
    handleInput state (1-player)

parseInt response str =
    let maybe_inp = readMaybe str :: Maybe Int
    in if isNothing maybe_inp
        then do
            putStrLn $ printf response str
            getInt response
        else do return . fromJust $ maybe_inp

getInt response = do
    inp <- getLine
    parseInt response inp

getInts response = do
    inp <- getLine
    let w = words inp
    mapM (parseInt response) w

getMove state player = do
    putStrLn $ "Player " ++ show player ++ "! Enter your move as <line> <position> <length>"
    ints <- getInts "Numbers should be integers but got %s"
    let [line, position, len] = ints
    f line position len
    where
    f line position len
        | line < 0 || line >= length state = do
            putStrLn $ "Line should be in range 0 to " ++ show (length state - 1)
            getMove state player
        | position < 0 || position > (length (state !! line) - 1) = do
            putStrLn $ "Position should be in range 0 to " ++ show (length (state !! line) - 1) ++ " and not already crossed out"
            getMove state player
        | otherwise = return (line, position, len)

wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'