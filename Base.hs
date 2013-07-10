{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Base where

import Control.Monad.State
import System.IO

data Function m = K
		| K1 (Function m)
		| S
		| S1 (Function m)
		| S2 (Function m) (Function m)
		| I
		| V
		| D
		| C
		| Dot Char
		| At
		| Pipe
		| Ask Char
		| Promise (Expression m)
		| Exit
		| Cont ((Function m) -> m (Function m))

data Expression m = Primitive (Function m)
		| App (Expression m) (Expression m)

class Monad m => UnlambdaEnv m where
	output :: Char -> m ()
	readChar :: m Bool
	currentChar :: m (Maybe Char)

type UnlambdaAction = State UnlambdaState

data UnlambdaState = UnlambdaState 
	{ currentCharacter :: Maybe Char
	, inputString :: String
	, reverseOutput :: String }
		deriving(Show)

outputPure :: Char -> UnlambdaAction ()
outputPure c = do
	s@(UnlambdaState {reverseOutput = o}) <- get
	put (s {reverseOutput = c:o})

readCharPure :: UnlambdaAction Bool
readCharPure = do
	s@(UnlambdaState {inputString = i}) <- get
	case i of
		"" -> put (s {currentCharacter = Nothing}) >> return False
		c:cs -> put (s {currentCharacter = Just c, inputString = cs}) >> return True

currentCharPure :: UnlambdaAction (Maybe Char)
currentCharPure = get >>= (return . currentCharacter)

instance UnlambdaEnv (State UnlambdaState) where
	output = outputPure
	readChar = readCharPure
	currentChar = currentCharPure

type UnlambdaIO = StateT (Maybe Char) IO 

instance UnlambdaEnv UnlambdaIO where
	output = liftIO . putChar
	readChar = liftIO isEOF >>= alternative (put Nothing >> return False) (liftIO getChar >>= (put . Just) >> return True)
	currentChar = get

evalCont :: (UnlambdaEnv m) => (Function m -> m (Function m)) -> Expression m -> m (Function m)
evalCont k (Primitive f) = k f
evalCont k (App f g) = evalCont newCont f
	where
		newCont D = k (Promise g)
		newCont f' = evalCont (\g' -> apply k f' g') g

eval :: UnlambdaEnv m => Expression m -> m (Function m)
eval = evalCont return

apply :: (UnlambdaEnv m) => (Function m -> m (Function m)) -> Function m -> Function m -> m (Function m)
apply k I g = k g
apply k V g = k V
apply k K g = k $ K1 g
apply k (K1 f) _ = k f
apply k S g = k $ S1 g
apply k (S1 f) g = k $ S2 f g
apply k (S2 f g) x = evalCont k $ App (App (Primitive f) (Primitive x)) (App (Primitive g) (Primitive x))
apply k (Dot c) x = output c >> k x
apply k (Promise e) x = evalCont (\f -> apply k f x) e 
apply k C f = evalCont k $ App (Primitive f) (Primitive (Cont k))
apply k (Cont c) x = c x
apply k At f = readChar >>= alternative (evalCont k (App (Primitive f) (Primitive I)))
					(evalCont k (App (Primitive f) (Primitive V)))
apply k Pipe f = do
		c <- currentChar
		case c of
			Just c' -> evalCont k (App (Primitive f) (Primitive (Dot c')))
			Nothing -> evalCont k (App (Primitive f) (Primitive V))
apply k (Ask c) f = do
		current <- currentChar
		case current of
			Nothing -> evalCont k (App (Primitive f) (Primitive V))
			Just c' -> if c == c'
					then evalCont k (App (Primitive f) (Primitive I))
					else evalCont k (App (Primitive f) (Primitive V))
apply k Exit x = return x

alternative :: a -> a -> Bool -> a
alternative x _ True = x
alternative _ y False = y

