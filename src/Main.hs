module Main where

import Prelude hiding (Word)

import Control.Monad
-- import Data.Char
-- import Data.Maybe
-- import Data.List
import Data.Set
import System.Exit
-- import System.Random
import System.Random.Shuffle

type Word = String
type WordList = [Word]

type UserInputs = Set Char
type GuessedInputs = Set Char
data Game = Game Word UserInputs GuessedInputs

instance Show Game where
	show (Game _ filled guessed) =
		"Filled: " ++ show filled
		++ " Guessed: " ++ show guessed

data GameStatus = GameWon | GameLost | GameUnknown

readWords :: IO WordList
readWords = do
	dict <- readFile "data/dict.txt"
	return $ lines dict

shuffleWords :: WordList -> IO WordList
shuffleWords xs = shuffleM xs

wordChars :: [Char] -> Set Char
wordChars word = fromList word

newGame :: IO Game
newGame = do
	myWords <- readWords
	shuffled <- shuffleWords myWords
	let word = shuffled !! 0
	let game = Game word empty empty
	return game

gameIsWon :: Game -> Bool
gameIsWon game = wordChars word == guessed
	where (Game word _ guessed) = game

gameIsLost :: Game -> Bool
gameIsLost game = filledLength > maxLength
	where
		wordLength = length $ wordChars word
		filledLength = length filled
		maxLength = wordLength * 75 `div` 100
		(Game word filled _) = game

checkGameStatus :: Game -> IO ()
checkGameStatus game = do
	let status = checkGameStatus' game
	case status of
		GameWon -> exitSuccess
		GameLost -> exitFailure
		GameUnknown -> return ()

checkGameStatus' :: Game -> GameStatus
checkGameStatus' game
	| gameIsWon game = GameWon
	| gameIsLost game = GameLost
	| otherwise = GameUnknown

runGame :: Game -> IO ()
runGame game = forever $ do
	checkGameStatus game
	putStrLn $ "Current puzzle is: " ++ show game
	guess <- getLine
	case guess of
		[c] -> return () --handleGuess puzzle c >>= runGame
		_ -> putStrLn "Your guess must\
		\ be a single character"


main :: IO ()
main = do
	putStrLn "Hello, World!"
	game <- newGame
	putStrLn "Let's start!"
	runGame game
