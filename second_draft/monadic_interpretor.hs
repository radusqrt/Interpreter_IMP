import DataTypes
import Data.Map as M
import Control.Applicative


data Item = Int_AR AExp | Int_BL BExp | Int_AST AST deriving Show

newtype Eval a = Eval {evalf :: (State -> Maybe (a, State))}
data State  = State {get_state :: M.Map String Integer} | NO_STATE deriving Show

instance Monad Eval where
    return = pure
    (>>=) (Eval interpreter) f =  (Eval (\initial_state -> case (interpreter initial_state) of
                        Just (result, state) -> ((evalf (f result)) state)
                        Nothing -> Nothing))

instance Functor Eval where
    fmap f (Eval p) = Eval (\s -> case p s of
                                    Just (x,s') -> Just ((f x),s')
                                    Nothing -> Nothing)

instance Applicative Eval where
   pure x = Eval (\s -> Just (x, s))
   (Eval pf) <*> (Eval p) = Eval (\s -> case pf s of
                            Nothing -> Nothing
                            Just (f,s') -> case p s' of
                                            Nothing -> Nothing
                                            Just (x, s'') -> Just (f x,s''))

instance Alternative Eval where
    empty = Eval (\s -> Nothing)
    (Eval p) <|> (Eval p') = Eval ( \s -> case p s of
                                Nothing -> p' s
                                x -> x)


evalInteger :: AExp -> Eval Integer
evalInteger (AOperation op1 op op2) = do {
    s1 <- (evalInteger op1);
    s2 <- (evalInteger op2);
    return ((get_ar_op op) s1 s2)
}
evalInteger (AValue int) = return int
evalInteger (AString str) = Eval (\s -> Just (((get_state s) M.! str) , s))

evalBoolean :: BExp -> Eval Bool
evalBoolean (BValue bool) = return bool
evalBoolean (BOperation bexp1 bop bexp2) = do {
    s1 <- (evalBoolean bexp1);
    s2 <- (evalBoolean bexp2);
    return ((get_bl_op bop) s1 s2)
}
evalBoolean (BCompare aexp1 baop aexp2) = do {
    s1 <- (evalInteger aexp1);
    s2 <- (evalInteger aexp2);
    return ((get_baop_op baop) s1 s2)
}

updateValue :: String -> Integer -> Eval State
updateValue k v = Eval (\s -> case k of
                                [] -> Nothing
                                key -> Just (NO_STATE, (State (M.insert key v (get_state s)))))


evalAST :: AST -> Eval State
evalAST (Asign str avalue) = do {
    value  <- (evalInteger avalue);
    (updateValue str value)
}


evalAST (Init [] ast) = do {
    context <- evalAST ast;
    return context
}

evalAST (Init (string:strings) ast) = do {
    updateValue string 0;
    context <- (evalAST (Init strings ast));
    return context;
}

evalAST (No_AST) = do {
    return (NO_STATE)
}

evalAST (If bexp ast1 ast2) = do {
    val <- (evalBoolean bexp);
    if val then
        do (evalAST ast1);
    else
        do (evalAST ast2);
}

evalAST (While bexp ast) = do {
    val <- (evalBoolean bexp);
    if val then
        do
            (evalAST ast);
            (evalAST (While bexp ast));
    else
        do
            return (NO_STATE);

}

evalAST (Instructions ast1 ast2) = do {
    (evalAST ast1);
    (evalAST ast2);
    return (NO_STATE);
}

get_ar_op :: AOp -> (Integer -> Integer -> Integer)
get_ar_op Plus = (+)
get_ar_op Minus = (-)

get_bl_op :: BLOp -> (Bool -> Bool -> Bool)
get_bl_op And = (&&)
get_bl_op Or = (||)

get_baop_op :: BAOp -> (Integer -> Integer -> Bool)
get_baop_op Greater = (>)
get_baop_op Lesser = (<)


main = do putStrLn (show test)
    where
        test = (evalf (evalAST ast)) (State M.empty)
        ast = (Init ["a", "b", "c"] (Instructions (Asign "a" (AValue 5)) (While (BCompare (AString "a") Greater (AValue 2)) (Asign "a" (AOperation (AString "a") Minus (AValue 1))))))

