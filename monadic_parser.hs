import Data.Maybe
import Data.Char
import Control.Applicative 
import Control.Monad (liftM, ap)

data AOp = Plus | Minus
data AExp = AOperation AExp AOp AExp | AValue Integer | AString String
--tipurile de date
data BExp = BCompare AExp BAOp AExp | BOperation BExp BLOp BExp | BValue Bool
data BLOp = And | Or 
data BAOp = Greater | Lesser
--AST-ul propriu zis
data AST  = Init [String] AST | Asign String AExp | If BExp AST AST | While BExp AST | Instructions AST AST | No_AST

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
charp c = (sat (== c)); 
    
charp_whitespace :: Char -> M [Char]
charp_whitespace c = do {
    e <- (star (charp c));
    whitespace;
    return e
}

get_content :: M a -> (String -> Maybe (a, String))
get_content (M var) = var
    
whitespace :: M String
whitespace = (star (charp ' '))

alphanumeric :: M String
alphanumeric = (plus (sat isAlpha))   

numeric :: M String
numeric = (plus (sat isNumber))

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

--boolean logic
bTrue :: M BExp
bTrue = do {
     string "True";
     return (BValue True) 
}

bFalse :: M BExp
bFalse = do {
    string "False";
    return (BValue False) 
}

--sigur pot generalizarea partea cu operatiile pentru a scoate codul duplicat

bAnd :: M BExp 
bAnd = do {
    (charp '(');
    bterm1 <- bOperations .||. bFalse .||. bTrue;
    whitespace;
    op <- string "&&";
    bterm2 <- bOperations .||. bFalse .||. bTrue;
    (charp ')');
    return (BOperation bterm1 And bterm2)
}



bOr :: M BExp 
bOr = do {
    (charp_whitespace '(');
    bterm1 <- bOperations .||. bFalse .||. bTrue;
    whitespace;
    op <- string "||";
    bterm2 <- bOperations .||. bFalse .||. bTrue;
    (charp ')');
    return (BOperation bterm1 Or bterm2)    
}

bOperations :: M BExp
bOperations = bOr .||. bAnd

b_arm_term :: M BExp
b_arm_term = do {
    (charp '(');
    bterm1 <- aTerm .||. aOperation;
    whitespace;
    bop <- charp '>' .||. charp '<';
    bterm2 <- aTerm .||. aOperation;
    (charp_whitespace ')');
    return (BCompare bterm1 (get_barm_operand bop) bterm2)
}

get_barm_operand :: Char -> BAOp 
get_barm_operand '>' = Greater
get_barm_operand '<' = Lesser


--arithmetic logic

aNumTerm :: M AExp
aNumTerm = do {
    whitespace;
    var <- numeric_parser;
    return (AValue (read var :: Integer))
}

aVarTerm :: M AExp
aVarTerm = do {
    whitespace;
    var <- variable_parser;
    return (AString var)
} 

aTerm :: M AExp 
aTerm = aVarTerm .||. aNumTerm

get_aritmetic_operand :: Char -> AOp 
get_aritmetic_operand '+' = Plus
get_aritmetic_operand '-' = Minus

aOperation :: M AExp
aOperation = do {
    (charp '(');
    aterm1 <- aOperation .||. aTerm;
    aop    <- aOperand;
    aterm2 <- aOperation .||. aTerm;
    (charp ')');
    return (AOperation aterm1 (get_aritmetic_operand aop) aterm2)  
}


aOperand :: M Char 
aOperand = charp '+' .||. charp '-'
 

asign_parser :: M AST
asign_parser = do {
    asigned_term <- variable_parser;
    charp '=';
    term <- aOperation .||. aTerm;
    return (Asign asigned_term term)
}

init_parser :: M AST 
init_parser = do {
    
}


-- main = do putStrLn (show new_AST)
--     where
--         new_AST = parse_syntax tokens
--         tokens = tokenize parsed
--         parsed = (filter_chars unparsed_cod)
--         unparsed_cod = 
--             "int s, n \n \
--             \ n = (1000 + (5 + 7)) \n \
--             \ s = (n + (3 + (2 + 7) - n + (s - 3)))\n"


instance Show AOp where
    show Plus  = "Plus"
    show Minus = "Minus"
 
instance Show BLOp where
    show And = "And"
    show Or  = "Or"

instance Show BAOp where
    show Greater = "Greater"
    show Lesser  = "Lesser"
 
instance Show AExp where
    show (AOperation exp1 op exp2) = "(AOperation "++ show exp1 ++ " " ++ show op  ++ " " ++ show exp2 ++ ")"
    show (AString str) = "(AString " ++ show str ++ ")"
    show (AValue val) = "(AValue " ++ show val ++ ")"

instance Show BExp where
    show (BValue boolean) = show boolean
    show (BCompare exp1 boolean arithmetic) = (show exp1) ++ (show boolean) ++ (show arithmetic)
    show (BOperation bool1 op bool2) = "(Boperation " ++ (show bool1) ++ " " ++ (show op) ++ " " ++ (show bool2) ++ ")"
 
instance Show AST where
    show (Init params remaining_AST) = "(Init " ++ (show params) ++ (show remaining_AST) ++ ")"
    show (Asign expr1 expr2) = "(Asign " ++ show expr1 ++ " " ++ show expr2 ++ ")"
    show (If bool_expr first_branch second_branch) = "(If " ++ (show bool_expr) ++ " " ++ (show first_branch) ++ " " ++ (show second_branch) ++ ")"
    show (Instructions next_instr remaining_AST) = "(Instructions" ++ (show next_instr) ++ " " ++ (show remaining_AST) ++ ")"
    show (While bool_expr while_AST) = "(while " ++ (show bool_expr) ++ (show while_AST) ++ ")"