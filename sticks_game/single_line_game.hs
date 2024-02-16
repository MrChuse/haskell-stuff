-- import Control.Monad
import Text.Read
import Data.Maybe
import Text.Printf
main = do
    putStrLn "Hello to my little sticks game\nEnter the number of sticks"
    startSticks <- getInt "Number of sticks should be integer but got \"%s\". Try again."
    putStrLn $ "Thanks! You've entered " ++ show startSticks ++ " sticks!"
    handleInput startSticks 0

handleInput 0 player = do putStrLn $ "Player " ++ show (1-player) ++ " won"
handleInput sticksLeft player = do
    putStrLn $ replicate sticksLeft '|'
    putStrLn $ "Player " ++ show player ++ "! Enter a number from 1 to 3 to subtract from sticks"
    sticksToSubtract <- getInt "Number of sticks should be integer but got \"%s\". Try again."
    if (sticksToSubtract < 1) || (sticksToSubtract > 3)
        then do
            putStrLn $ show sticksToSubtract ++ " is not in range from 1 to 3. Try again."
            handleInput sticksLeft player
        else do
            if sticksToSubtract > sticksLeft
                then do
                    putStrLn $ "You tried to subtract " ++ show sticksToSubtract ++ " sticks but only " ++ show sticksLeft ++ " left"
                    handleInput sticksLeft player
                else do
                    handleInput (sticksLeft - sticksToSubtract) (1-player)

getInt response = do
    inp <- getLine
    let maybe_inp = readMaybe inp :: Maybe Int
    if isNothing maybe_inp
        then do
            putStrLn $ printf response inp
            getInt response
        else do return . fromJust $ maybe_inp
