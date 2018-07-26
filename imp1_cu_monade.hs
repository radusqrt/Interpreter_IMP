{-

    - Codul e usor de extins cu operatii noi
    - nu foarte eficient dpdv al starii

-}


type Var = String

data AExp = AVal Integer | AVar Var 
              | Slash AExp AExp
              | Plus AExp AExp           -- precedence is not covered in the implementation. It should be the task of the parser
              | APar AExp deriving Show

data BExp = BVal Bool 
             | Leq AExp AExp
             | Not BExp
             | And BExp BExp -- must encode precedence
             | BPar BExp deriving Show

-- it seems to complicate the semantics - two functions would be necessary
--data Block = VoidBlock | Block Stmt deriving Show

data Stmt = VoidBlock |
            Block Stmt |
            Assign Var AExp |
            If BExp Stmt Stmt |
            While BExp Stmt |
            Seq Stmt Stmt deriving Show


-- program
data Imp = Imp [Var] Stmt deriving Show            

sumimp = Imp ["s","n"] $ Seq (Assign "n" (AVal 10000)) $ While ((AVal 0) `Leq` (AVar "n")) $ Block $ Seq (Assign "s" ((AVar "s") `Plus` (AVar "n"))) (Assign "n" ((AVar "n") `Plus` (AVal (-1))))

type State = [(Var,Integer)]


-- similar to Maybe but 'errors' also have a message
-- data Result a = Result a | Error String deriving Show

type M a = Maybe a

-- un tip monadic este un functor (* => *)

class Monad M where
  return :: a -> M a
  return x = Just x
  (>>=) :: M a -> (a -> M b) -> M b
  m (>>=) f = case m of
                Just x -> f x
                Nothing -> Nothing 
                


-- auxiliary functions where speed improvements may be done
update :: Var -> Integer -> State -> State
update v e s = (v,e) : (filter (\(x,_)->x /= v) s)

-- checks if variable is defined in state
has :: State -> Var -> Bool
has s v = (filter (\(x,_)->x==v) s) /= []

------------------
-- error messages
undeclared v = Error ("Variable "++v++" is undeclared \n")


evalAExp :: State -> AExp -> M Integer

evalAExp s (AVal i) = return i
evalAExp s (AVar v) = if s `has` v then return $ snd $ head $ filter (\(x,_)-> x == v) s else Nothing

-- exemplu de folosire a slash
evalAExp s (e `Slash` e') = 
   (evalAExp s e) >>= (\x -> (evalAExp s e') >>= (\y-> ret (x / y))))


{-
  case ((evalAExp s e),(evalAExp s e')) of
    (Error m,_) -> Error m
    (_,Error m) -> Error m
    (Result v,Result v') -> Result (v `div` v')
}
evalAExp s (e `Plus` e') = 
  case ((evalAExp s e),(evalAExp s e')) of
    (Error m,_) -> Error m
    (_,Error m) -> Error m
    (Result v,Result v') -> Result (v + v')
evalAExp s (APar e) = (evalAExp s e) 
-}

evalBExp :: State -> BExp -> Result Bool
evalBExp s (BVal b) = Result b
evalBExp s (e `Leq` e') = case ((evalAExp s e),(evalAExp s e')) of
                            (Error m,_) -> Error m
                            (_,Error m) -> Error m
                            (Result i, Result i') -> Result (i<=i')
evalBExp s (Not e) = case evalBExp s e of
                        Error m -> Error m
                        Result b -> Result (not b)
evalBExp s (e `And` e') = case (evalBExp s e) of
                            Error m -> Error m
                            Result False -> Result False
                            Result True -> evalBExp s e'
evalBExp s (BPar e) = evalBExp s e     



eval :: Imp -> Result State
eval (Imp l p) = let
  ev :: State -> Stmt -> Result State
  ev s VoidBlock = Result s
  -- ignore the { } surrounding a statement
  ev s (Block stmt) = ev s stmt
  -- evaluate e in s, then update v in s with the result; if v is not defined in s - report
  ev s (Assign v e) = if (s `has` v) then
                            case (evalAExp s e) of
                              Error m -> Error m
                              Result r -> Result (update v r s)
                      else undeclared v 
  ev s (If c th el) = case evalBExp s c of
                        Error m -> Error m
                        Result True -> ev s th
                        Result False -> ev s el
  ev s (While c stmt) = ev s (If c (Seq stmt (While c stmt)) VoidBlock)
{-
                      case evalBExp s c of
                          Error m -> Error m
                          Result True -> ev s (Seq stmt (While c stmt))
                          Result False -> Result s  
                          -}                  
  ev s (Seq stmt stmt') = case ev s stmt of
                            Error m -> Error m
                            Result s' -> ev s' stmt'                     
  -- build the initial state and evaluate the program
  in ev (map (\x->(x,0)) l) p  

main = putStrLn $ show $ eval sumimp


