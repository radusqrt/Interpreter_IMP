import Prelude hiding ((>>=), return)
import Data.Maybe
import Data.Char

newtype M a = M (String -> Maybe (a, String))


class ParserMonad m where
	--monad logic
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b
    get_content :: m a -> (String -> Maybe (a, String))
    (.||.) :: m a -> m a -> m a
    --regex logic
    star :: m a -> m [a]
    plus :: m a -> m [a]
    --parser logic
    sat :: (Char -> Bool) -> m Char
    charp :: Char -> m Char
	    


instance ParserMonad M where	
	return x = M (\s -> Just (x, s))

	(>>=) (M p) f =  (M (\s -> case (p s) of
						Just (v, s') -> ((get_content (f v)) s')
						Nothing -> Nothing))

	(.||.) (M p)  (M p') = M (\s -> case p s of
					Just (v, s') -> Just (v, s')
					Nothing      -> p' s)

	get_content (M var) = var
	
	plus p = p >>=  (\x -> star p >>= (\y -> return (x:y)))
	
	star p = plus p .||. return []

 	sat p = M (\s -> case s of 
 					[] -> Nothing
 					x  -> case (p (head x)) of 
 							True -> Just ((head x), (tail x))
 							False -> Nothing)

 	charp c = (sat (== c))

whitespace :: M String
whitespace = (star (charp ' '))

alphanumeric :: M String
alphanumeric = (star (sat isAlpha))

whitespace_and_alphanumeric :: M String
whitespace_and_alphanumeric = alphanumeric 