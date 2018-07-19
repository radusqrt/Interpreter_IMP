{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where
import Data.Bool
import Data.Map as M

class Interpretor a where
    validate_AST :: a -> Bool 
    get_next_state :: (a, M.Map String AExp) -> (a, M.Map String AExp)

class Operation data_type return_type operations where
    operation :: data_type -> operations -> data_type -> return_type

class Expand_Symbol data_type where
    expand :: data_type -> M.Map String data_type -> data_type

--operatiile peste tipurile de date
data AOp = Plus | Minus
data AExp = AOperation AExp AOp AExp | AValue Integer | AString String
--tipurile de date
data BExp = BCompare AExp BAOp AExp | BOperation BExp BLOp BExp | BValue Bool
data BLOp = And | Or 
data BAOp = Greater | Lesser
--AST-ul propriu zis
data AST  = Init [String] AST | Asign String AExp | If BExp AST AST | While BExp AST | Instructions AST AST | No_AST
 
 
instance Expand_Symbol AExp where
    expand value@(AValue int) symb_map = value  
    expand (AString symb) symb_map = (symb_map M.! symb)
    expand (AOperation exp1 op exp2) symb_map = (AOperation (expand exp1 symb_map) op (expand exp2 symb_map))

instance Operation AExp AExp AOp where
    --operatiile alese
    operation exp1@(AValue a) Plus exp2@(AValue b) = AValue (a + b)
    operation exp1@(AValue a) Minus exp2@(AValue b) = AValue (a - b)
    operation exp1@(AOperation argv11 argv12 argv13) op exp2@(AOperation argv21 argv22 argv23) = operation (operation argv11 argv12 argv13) op exp2
    operation exp1 op exp2@(AOperation argv1 argv2 argv3) = operation exp1 op (operation argv1 argv2 argv3)
    operation exp1@(AOperation argv1 argv2 argv3) op exp2 = operation (operation argv1 argv2 argv3) op exp2 
      
instance Operation BExp BExp BLOp where
    operation exp1@(BValue a) And exp2@(BValue b) = BValue (a && b)
    operation exp1@(BValue a) Or exp2@(BValue b) = BValue (a || b)
    operation exp1@(BOperation argv11 argv12 argv13) op exp2@(BOperation argv21 argv22 argv23) = operation (operation argv11 argv12 argv13) op exp2
    operation exp1@(BOperation argv1 argv2 argv3) op exp2 = operation (operation argv1 argv2 argv3) op exp2
    operation exp1 op exp2@(BOperation argv1 argv2 argv3) = operation exp1 op (operation argv1 argv2 argv3)
    operation exp1@(BCompare argv11 argv12 argv13) op exp2@(BCompare argv21 argv22 argv23) = operation (operation argv11 argv12 argv13) op exp2
    operation exp1 op exp2@(BCompare argv1 argv2 argv3) = operation exp1 op (operation argv1 argv2 argv3)
    operation exp1@(BCompare argv1 argv2 argv3) op exp2 = operation (operation argv1 argv2 argv3) op exp2 

instance Operation AExp BExp BAOp where
    operation exp1@(AValue a) Greater exp2@(AValue b) = BValue (a > b)
    operation exp1@(AValue a) Lesser exp2@(AValue b) = BValue (a < b)

instance Interpretor AST where
    --folosit pentru validarea unui AST
    validate_AST (Init _ remaining_AST) = validate_AST remaining_AST
    validate_AST (Asign _ _) = True
    validate_AST (If _ first_branch second_branch) = validate_AST first_branch && validate_AST second_branch
    validate_AST (Instructions next_instr remaining_AST) = validate_AST next_instr && validate_AST remaining_AST
    validate_AST (While _ while_AST) = validate_AST while_AST
    --initializez by default cu 0
    get_next_state ((Init params remaining_AST), _) =  get_next_state (remaining_AST, M.fromList (zip params (repeat (AValue 0))))
    get_next_state ((Asign symbol exp1@(AValue number)), symbol_map) = (No_AST , M.insert symbol exp1 symbol_map)
    get_next_state ((Asign symbol exp1@(AString assign_to_symbol)), symbol_map) = (No_AST, M.insert symbol (symbol_map M.! assign_to_symbol) symbol_map)
    get_next_state ((Asign symbol exp1@(AOperation _ _ _)), symbol_map) = (No_AST, M.insert symbol (operation argv1 argv2 argv3) symbol_map)
        where
            (AOperation argv1 argv2 argv3) = expand exp1 symbol_map
    get_next_state ((Instructions block1 block2), symbol_map) = get_next_state (block2, new_symbol_map)
        where
            (_, new_symbol_map) = get_next_state (block1, symbol_map)
    get_next_state ((If ))

instance Show AOp where
    show Plus  = "+"
    show Minus = "-"
 
instance Show BLOp where
    show And = "&&"
    show Or  = "||"

instance Show BAOp where
    show Greater = ">"
    show Lesser  = "<"
 
instance Show AExp where
    show (AOperation exp1 op exp2) = show exp1 ++ " " ++ show op  ++ " " ++ show exp2
    show (AValue number) = show number
 
instance Show BExp where
    show (BValue boolean) = show boolean
    show (BCompare exp1 boolean arithmetic) = (show exp1) ++ (show boolean) ++ (show arithmetic)
    show (BOperation bool1 op bool2) = (show bool1) ++ (show op) ++ (show bool2)
 
instance Show AST where
    show (Init params remaining_AST) = "(Init " ++ (show params) ++ (show remaining_AST) ++ ")"
    show (Asign expr1 expr2) = "(Asign " ++ show expr1 ++ " " ++ show expr2 ++ ")"
    show (If bool_expr first_branch second_branch) = "(If " ++ (show bool_expr) ++ " " ++ (show first_branch) ++ " " ++ (show second_branch) ++ ")"
    show (Instructions next_instr remaining_AST) = " " ++ (show next_instr) ++ " " ++ (show remaining_AST)
    show (While bool_expr while_AST) = "(while " ++ (show bool_expr) ++ (show while_AST) ++ ")"
 
 

main = do putStrLn (show (M.toList final_map))
    where
        (final_state, final_map) = (get_next_state ((Init ["n", "s"] (Instructions (Asign "n" (AValue 5)) (Asign "s" (AOperation (AValue 5) Plus (AString "n"))))), M.fromList []))
    --do putStrLn (show (operation (AOperation (AOperation (AString "n") Plus (AValue 3)) Plus (AOperation (AString "m") Minus (AValue 2))) Plus (AOperation (AValue 3) Plus (AValue 5)) (M.fromList [("n", (AValue 2)), ("m", (AValue 5))])))
    --where
        --current_AST = (Init ["n", "s"] (Instructions (Asign "n" (AValue 5)) (While (BValue True) (Instructions (Asign "s" (AOperation (AValue 5) Plus (AValue 2))) (Asign "s" (AOperation (AValue 1) Minus (AValue 2)))))))
--test:
--putStrLn (show (Root (Init [] (Instructions (Asign "n" (AValue 5)) (While (BValue True) (Instructions (Asign "s" (AOperation (AValue 5) Plus (AValue 2))) (Asign "s" (AOperation (AValue 1) Minus (AValue 2)))))))))
--mai astept pana sa fac operatii pe Var, maine ma ocup.