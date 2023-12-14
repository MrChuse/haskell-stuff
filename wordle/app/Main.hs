module Main where

import System.Console.ANSI
import System.IO
import Control.Monad
import System.Random

main :: IO ()
main = do
    answers <- splitFile "answerlist.txt"
    acceptableWords <- splitFile "wordlist.txt"

    gen <- getStdGen
    let todaysWord = getWordFromList answers gen
    -- putStr "Today's word is "
    -- printWordWithColors (replicate 5 Red) todaysWord
    -- putChar '\n'
    putStrLn "Guess the word game! It has 5 letters."

    playInteractiveGame [] todaysWord acceptableWords

    return ()


splitFile filename = do
    contents <- readFile filename
    return . words $  contents

printWordWithColors [] [] = setSGR [Reset]
printWordWithColors colors letters
    | length colors /= length letters = error "Called printWordWithColors with colors and letters of different length"
printWordWithColors (color:colors) (letter:letters) = do
    setSGR [SetColor Foreground Vivid color]
    putChar letter
    printWordWithColors colors letters

getWordFromList l gen = l !! fst (randomR (0, length l) gen)

tryGuess guesses correctWord acceptableWords = do
    showGuesses correctWord guesses
    putStrLn "Type your guess!"
    line <- getLine
    if line `notElem` acceptableWords
        then do
            putStrLn $ line ++ " is not present in acceptable words list. Try again."
            playInteractiveGame guesses correctWord acceptableWords
        else playInteractiveGame (line:guesses) correctWord acceptableWords

playInteractiveGame [] correctWord acceptableWords = tryGuess [] correctWord acceptableWords
playInteractiveGame guesses@(lastGuess:_) correctWord acceptableWords
    | lastGuess == correctWord = do
        showGuesses correctWord guesses
        putStrLn "You won!"
    | length guesses > 5 = do
        showGuesses correctWord guesses
        putStrLn $ "You lost. The correct word was " ++ correctWord
    | otherwise = tryGuess guesses correctWord acceptableWords

showBoxOpen size = putStrLn $ "┌" ++ replicate size '─' ++ "┐"
showBoxClose size = putStrLn $ "└" ++ replicate size '─' ++ "┘"
showGuess correctWord guess = do
    let colors = getColorsOfGuess correctWord guess
    putChar '│'
    printWordWithColors colors guess
    putChar '│'
    putChar '\n'

showGuesses correctWord [] = do
    let s = "│No guesses yet│"
    showBoxOpen $ length s - 2
    putStrLn s
    showBoxClose $ length s - 2
showGuesses correctWord guesses = do
    showBoxOpen 5
    mapM_ (showGuess correctWord) guesses
    showBoxClose 5

getColorOfLetter correctWord position letter
  | correctWord !! position == letter = Green
  | letter `elem` correctWord = Yellow
  | otherwise = White

getColorsOfGuess correctWord guess = [getColorOfLetter correctWord position letter | (position, letter) <- zip [0..] guess]
