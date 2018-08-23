
{-
	Ce e un parser? urmareste o strategie (data de gramatica)
	String -> Maybe (a, String)

	Ce este un interpretor? urmareste o strategie (data de un AST)
	State -> Maybe (a, State)

-}

data State = ...

newtype Eval a = Eval (State -> Maybe (a, State))

class Eval a where
	eval :: Item -> Eval a

instance Eval Bool where
	-- evaluarea Itemilor care intorc booleni


{-
evalAST :: AST -> Eval ? 
evalAST (Instructions p1 p2) = do {evalAST p1; evalAST p2}

evalAExp :: AExp -> Eval Integer
evalAExp (AOperation e op e') = do {
					i <- evalAExp e;
					i' <- evalAExp e';
					return (i `(get_op op)` i')
					} 
-}


run :: AST -> State
run p = eval p initialState	



