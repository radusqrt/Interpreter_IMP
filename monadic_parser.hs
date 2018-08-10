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
    

parse_clean :: M a -> M a 
parse_clean parser = do {
    star ((string "\n") .||. (string " "));
    e <- parser;
    star ((string "\n") .||. (string " "));
    return e
}

string :: String -> M String
string (x:xs) = (charp x) >>= (\c -> string xs >>= (\str -> return (c:str))) 
string [] = return []

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
    e <- (parse_clean numeric);
    return e 
}

-- --boolean logic
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


bl_terms_parse :: M BExp
bl_terms_parse = do {
    term <- parse_clean (bl_expr .||. bTrue .||. bFalse);
    b_op <- parse_clean (string "&&" .||. string "||");
    remaining_terms <- (bl_terms_parse .||. bl_expr .||. bTrue .||. bFalse);
    return (BOperation term (get_bop b_op) remaining_terms)
}

bl_expr :: M BExp 
bl_expr = do {
    parse_clean (string "(");
    bterm1 <- parse_clean (bl_expr .||. bl_terms_parse .||. bTrue .||. bFalse);
    parse_clean (string ")");
    return bterm1
}

get_bop :: String -> BLOp
get_bop "&&" = And
get_bop "||" = Or

b_expr :: M BExp
b_expr =  bl_expr .||. ba_expr

-- --arithmetic logic

aNum :: M AExp
aNum = do {
    var <- (parse_clean numeric_parser);
    return (AValue (read var :: Integer))
}

aVar :: M AExp
aVar = do {
    var <- (parse_clean variable_parser);
    return (AString var)
} 

a_ops :: M String
a_ops = string "+" .||. string "-" 

a_terms_parse :: M AExp
a_terms_parse = do {
    term <- parse_clean (a_expr .||. aNum .||. aVar);
    a_op <- parse_clean a_ops;
    remaining_terms <- parse_clean (a_terms_parse .||. a_expr .||. aNum .||. aVar);
    return (AOperation term (get_aop a_op) remaining_terms)
}

a_expr :: M AExp
a_expr = do {
    parse_clean (charp '(');
    aterm <- parse_clean (a_expr .||. a_terms_parse .||. aNum .||. aVar);
    parse_clean (charp ')');    
    return aterm  
}

get_aop :: String -> AOp 
get_aop "+" = Plus
get_aop "-" = Minus

ba_expr :: M BExp
ba_expr = do {
    parse_clean (string "(");
    bterm1 <- parse_clean (a_expr .||. aVar .||. aNum);
    baop <- parse_clean (string ">" .||. string "<");
    bterm2 <- parse_clean (a_expr .||. aVar .||. aNum);
    parse_clean (string ")");
    return (BCompare bterm1 (get_baop baop) bterm2)
}

get_baop :: String -> BAOp 
get_baop ">" = Greater
get_baop "<" = Lesser



asign_parser :: M AST
asign_parser = do {
    asigned_term <- parse_clean variable_parser;
    parse_clean (charp '=');
    term <- parse_clean (a_expr .||. aNum .||. aVar);
    return (Asign asigned_term term)
}

init_terms :: M String
init_terms = do {
    var <- parse_clean alphanumeric;
    parse_clean (string ",");
    return var
}

init_parser :: M AST 
init_parser = do {
    parse_clean (string "int");
    vars <- star init_terms;
    last_var <- parse_clean alphanumeric;
    ast  <- instructions_parser;
    return (Init (last_var:vars) ast)
}

operations_parser :: M AST 
operations_parser = while_expr .||. if_expr .||. asign_parser

if_expr :: M AST
if_expr = do {
    parse_clean (string "if");
    b_op <- b_expr;
    parse_clean (charp '{');
    ast1 <- instructions_parser;
    parse_clean (charp '}');
    parse_clean (string "else");
    parse_clean (charp '{');
    ast2 <- instructions_parser;
    parse_clean (charp '}');
    return (If b_op ast1 ast2)
}

while_expr :: M AST
while_expr = do {
    parse_clean (string "while");
    b_op <- parse_clean b_expr;
    parse_clean (string "{");
    ast <- instructions_parser;
    parse_clean (string "}");
    return (While b_op ast)
}

instructions_parser :: M AST 
instructions_parser = do {
    instr1 <- operations_parser;
    instr2 <- instructions_parser .||. (return (No_AST));
    return (Instructions instr1 instr2)
}




-- --debug stuff
get_maybe :: Maybe (AST, String) -> AST
get_maybe Nothing = No_AST
get_maybe (Just (ast, str)) = ast

get_maybe_str :: Maybe (AST, String) -> String
get_maybe_str Nothing = "Nothing"
get_maybe_str (Just (ast,str)) = str




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
    show (BCompare exp1 boolean arithmetic) = "(BCompare " ++ (show exp1) ++ (show boolean) ++ (show arithmetic) ++ ")"
    show (BOperation bool1 op bool2) = "(Boperation " ++ (show bool1) ++ " " ++ (show op) ++ " " ++ (show bool2) ++ ")"
 
instance Show AST where
    show (Init params remaining_AST) = "(Init " ++ (show params) ++ (show remaining_AST) ++ ")"
    show (Asign expr1 expr2) = "(Asign " ++ show expr1 ++ " " ++ show expr2 ++ ")"
    show (If bool_expr first_branch second_branch) = "(If " ++ (show bool_expr) ++ " " ++ (show first_branch) ++ " " ++ (show second_branch) ++ ")"
    show (Instructions next_instr remaining_AST) = "(Instructions" ++ (show next_instr) ++ " " ++ (show remaining_AST) ++ ")"
    show (While bool_expr while_AST) = "(while " ++ (show bool_expr) ++ (show while_AST) ++ ")"
    show (No_AST) = " No_AST "