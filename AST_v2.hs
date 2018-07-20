{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where
import Data.Bool
import Data.Map as M


--clasa pentru definirea unui validator pentru AST-ul ales
--get_next_state momentan  returneaza starea finala, il voi face sa mearga pas cu pas prin instructiuni
class Interpretor a where
    validate_AST :: a -> Bool 
    get_next_state :: (a, M.Map String AExp) -> (a, M.Map String AExp)

--clasa pentru definirea unei operatii: input, return type si operatiile permise peste 
class Operation data_type return_type operations where
    operation :: data_type -> operations -> data_type -> return_type

--clasa pentru definirea simbolurilor si pentru efectuarea operatiilor, in prima varianta lasasem totul in
--Interpretor toata logica, cred ca este mai ok asa.
class Expand_Symbol data_type symbol_type where
    expand :: data_type -> M.Map String symbol_type -> data_type
    choose_operation :: data_type -> M.Map String symbol_type -> data_type


--operatiile peste tipurile de date
data AOp = Plus | Minus
data AExp = AOperation AExp AOp AExp | AValue Integer | AString String
--tipurile de date
data BExp = BCompare AExp BAOp AExp | BOperation BExp BLOp BExp | BValue Bool
data BLOp = And | Or 
data BAOp = Greater | Lesser
--AST-ul propriu zis
data AST  = Init [String] AST | Asign String AExp | If BExp AST AST | While BExp AST | Instructions AST AST | No_AST
 
 --singura clasa care foloseste variabilele pentru a asocia cu valorile
instance Expand_Symbol AExp AExp where
    expand value@(AValue int) symb_map = value  
    expand (AString symb) symb_map = (symb_map M.! symb)
    expand (AOperation exp1 op exp2) symb_map = (AOperation (expand exp1 symb_map) op (expand exp2 symb_map))
    choose_operation value@(AValue int) symb_map = value
    choose_operation value@(AString symb) symb_map = expand value symb_map 
    choose_operation value@(AOperation exp1 op exp2) symb_map = operation clean_exp1 clean_op clean_exp2 
        where 
            (AOperation clean_exp1 clean_op clean_exp2) = expand value symb_map

--nu exista variabile de tip bool, dar o expresie bool cand o desfac poate sa gaseasca si variabile (BCompare)
instance Expand_Symbol BExp AExp where
    expand value@(BValue val) _ = value
    expand value@(BCompare art1 op art2) symb_map = (BCompare (expand art1 symb_map) op (expand art2 symb_map))
    expand value@(BOperation bop1 op bop2) _ = value
    choose_operation expr@(BValue value) _ = expr
    choose_operation expr@(BOperation argv1 argv2 argv3) _ = operation argv1 argv2 argv3
    choose_operation expr@(BCompare argv1 argv2 argv3) symb_map = operation c_argv1 c_argv2 c_argv3
        where
        (BCompare c_argv1 c_argv2 c_argv3) = expand expr symb_map 

instance Operation AExp AExp AOp where
    --operatiile alese
    operation exp1@(AValue a) Plus exp2@(AValue b) = AValue (a + b)
    operation exp1@(AValue a) Minus exp2@(AValue b) = AValue (a - b)
    operation exp1@(AOperation argv11 argv12 argv13) op exp2@(AOperation argv21 argv22 argv23) = operation (operation argv11 argv12 argv13) op exp2
    operation exp1 op exp2@(AOperation argv1 argv2 argv3) = operation exp1 op (operation argv1 argv2 argv3)
    operation exp1@(AOperation argv1 argv2 argv3) op exp2 = operation (operation argv1 argv2 argv3) op exp2 

-- operatori de tip bool peste termeni de tip bool cu return type tot bool_expr. Daca gaseste in interiorul ei operatori aritmetici se duce corect
-- in clasa de mai jos, termeni aritmetici cu operator boolean si return_type boolean     
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
    --initializez by default cu 0 toate variabilele
    get_next_state ((Init params remaining_AST), _) =  get_next_state (remaining_AST, M.fromList (zip params (repeat (AValue 0))))
    get_next_state ((Asign symbol exp1), symbol_map) = (No_AST , M.insert symbol (choose_operation exp1 symbol_map) symbol_map)
    get_next_state ((Instructions block1 block2), symbol_map) = get_next_state (block2, new_symbol_map)
        where
            (_, new_symbol_map) = get_next_state (block1, symbol_map)
    get_next_state ((If bexpr ast_true ast_false), symbol_map) = if truth_value 
        then get_next_state (ast_true, symbol_map)
        else get_next_state (ast_false, symbol_map)
        where
            (BValue truth_value) = choose_operation bexpr symbol_map
    get_next_state ((While bexpr while_ast), symbol_map) = if truth_value
        then get_next_state ((While bexpr while_ast), new_symbol_map)
        else ((No_AST, symbol_map))
        where
            (BValue truth_value) = choose_operation bexpr symbol_map
            (_, new_symbol_map)  = get_next_state (while_ast, symbol_map)
             
    get_next_state (No_AST, symbol_map) = (No_AST, symbol_map)    
    --get_next_state ((While bexpr ast_true ast_false))

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
        (final_state, final_map) = (get_next_state ((Init ["n", "s"] (Instructions (Asign "n" (AValue 100)) (While (BCompare (AValue 0) Lesser (AString "n"))  (Instructions (Asign "s" (AOperation (AString "s") Plus (AString "n"))) (Asign "n" (AOperation (AString "n") Plus (AValue (-1)))))))), M.empty))
        --(final_state, final_map) = (get_next_state ((Init ["n", "s"] (If (BCompare (AValue 3) Greater (AValue 2)) (Asign "n" (AValue 5)) (While (BCompare (AString "n") Lesser (AValue 5)) (Asign "n" (AOperation (AString "n") Plus (AValue 1)))))), M.empty))