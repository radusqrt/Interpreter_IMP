import Data.Maybe
import Data.Char
import Control.Applicative 
import Control.Monad (liftM, ap)

newtype M a = M (String -> Maybe (a, String))

instance Functor M where
  fmap = liftM

instance Applicative M where
  pure  = return
  (<*>) = ap

instance Monad M where    
    return x = M (\s -> Just (x, s))
    (>>=) (M p) f =  (M (\s -> case (p s) of
                        Just (v, s') -> ((get_content (f v)) s')
                        Nothing -> Nothing))



star :: M a -> M [a]
star p = plus p .||. return []

plus :: M a -> M [a]
plus p = p >>=  (\x -> star p >>= (\y -> return (x:y)))

--beta ideas
and_one :: M a ->  M a -> M [a]
and_one p p' = p >>= (\x -> p' >>= (\y -> return (x:[y])))

and_multiple :: M [a] -> M a -> M [a]
and_multiple p p' = p >>= (\x -> p' >>= (\y -> return (x ++ [y])))

--string :: String -> M String 
--string str =  
--    where 
--        char_parsers = (map (charp) str)

sat :: (Char -> Bool) -> M Char
sat p = M (\s -> case s of 
                    [] -> Nothing
                    x  -> case (p (head x)) of 
                            True -> Just ((head x), (tail x))
                            False -> Nothing)

(.||.) :: M a -> M a -> M a
(.||.) (M p)  (M p') = M (\s -> case p s of
                    Just (v, s') -> Just (v, s')
                    Nothing      -> p' s)

charp :: Char -> M Char
charp c = (sat (== c))

get_content :: M a -> (String -> Maybe (a, String))
get_content (M var) = var
    
whitespace :: M String
whitespace = (star (charp ' '))

alphanumeric :: M String
alphanumeric = (star (sat isAlpha))   

numeric :: M String
numeric = (star (sat isNumber))

variable_parser :: M String
variable_parser = do {
    e  <- alphanumeric;    
    whitespace;
    return e
}

validate_number :: String -> Bool
validate_number [] = True
validate_number (x:xs) = if (isNumber x)
    then validate_number xs
    else False

numeric_parser :: M String
numeric_parser = do {
    e <- numeric;
    whitespace;
    return e 
}

string :: String -> M String
string (x:xs) = (charp x) >>= (\c -> string xs >>= (\str -> return (c:str))) 
string [] = whitespace

-- bTrue :: M BExp
-- bTrue = do {
--     string "True";
--     return (BVal True)
-- }

-- bFalse :: M BExp
-- bFalse = do {
--     string "False";
--     return (BVal False)
-- }

-- bAnd :: M BExp 
-- bAnd = do {
--     bterm1 <- bFalse .||. bTrue;
--     op <-string "&&";
--     bterm2 <- ebFalse .||. bTrue;
--     return (BOperation bterm1 And bterm2)
-- }



-- bExp :: BExp 
-- bExp = do {
    
-- }