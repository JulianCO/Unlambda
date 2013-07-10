module Main where

import Base
import Text.Parsec
import Data.Text
import Text.Parsec.Text
import System.Environment
import Control.Monad.State
import qualified Data.Text.IO as T

parsePrimitive :: UnlambdaEnv m => Parser (Expression m)
parsePrimitive = do
	c <- oneOf "skivdce@|.?r"
	case c of
		's' -> return (Primitive S)
		'k' -> return (Primitive K)
		'i' -> return (Primitive I)
		'v' -> return (Primitive V)
		'd' -> return (Primitive D)
		'c' -> return (Primitive C)
		'e' -> return (Primitive Exit)
		'@' -> return (Primitive At)
		'|' -> return (Primitive Pipe)
		'.' -> anyChar >>= return . Primitive . Dot
		'?' -> anyChar >>= return . Primitive . Ask
		'r' -> return (Primitive (Dot '\n'))

parseApp :: UnlambdaEnv m => Parser (Expression m)
parseApp = do
	char '`'
	f <- parseExpression
	g <- parseExpression
	return $ App f g

parseComment :: Parser Char
parseComment = do
	char '#'
	many (noneOf "\n")
	char '\n'

parseExpression :: UnlambdaEnv m => Parser (Expression m)
parseExpression = do
	skipMany (parseComment <|> space)
	parsePrimitive <|> parseApp

main = do
	args <- getArgs
	case args of
		[] -> putStrLn "Usage: unlambda <file>"
		(file:_) -> do
			contents <- T.readFile file 
			case parse parseExpression file contents of
				Left e -> print e
				Right exp -> runStateT (eval exp) (Nothing :: Maybe Char) >> return ()

