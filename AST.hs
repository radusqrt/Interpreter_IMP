{-# LANGUAGE MultiParamTypeClasses #-}
 
import Data.Bool
 
--operatiile peste tipurile de date
data AOp = Plus | Minus
data AExp = AOperation AExp AOp AExp | AValue Integer | AString String
--tipurile de date
data BExp = BOperation BExp BOp BExp | BValue Bool | AExp BOp AExp | BString --variabile ca boolean nu cred ca exista momentan  
data BOp = And | Or | Greater | Lesser
--AST-ul propriu zis
data AST  = Root AST | Init [String] AST | Asign String AExp | If BExp AST AST | While BExp AST | Instructions AST AST
 
 -- to do, cum fac sa mearga ok fara sa hardcodez pe vars
 
class Operation data_type operations where
    operation :: data_type -> operations -> data_type -> data_type
 
instance Operation AExp AOp where
    operation (AValue a) Plus (AValue b) = AValue (a + b)
    operation (AValue a) Minus (AValue b) = AValue (a - b)
    operation exp1@(AOperation argv1 argv2 argv3) op exp2@(AValue _) = operation (operation argv1 argv2 argv3) op exp2
    operation exp1@(AValue _) op exp2@(AOperation argv1 argv2 argv3) = operation exp1 op (operation argv1 argv2 argv3)
    operation exp1@(AOperation argv11 argv21 argv31) op exp2@(AOperation argv12 argv22 argv32) = operation (operation argv11 argv21 argv31) op (operation argv12 argv22 argv32)
 
--TO DO:instance Operation BExp BOp where
 
 
-- functie de validat BST-ul pana acum
validate_AST :: AST -> Bool
validate_AST (Root remaining_AST) = validate_AST remaining_AST
validate_AST (Init _ remaining_AST) = validate_AST remaining_AST
validate_AST (Asign _ _) = True
validate_AST (If _ first_branch second_branch) = validate_AST first_branch && validate_AST second_branch
validate_AST (Instructions next_instr remaining_AST) = validate_AST next_instr && validate_AST remaining_AST
validate_AST (While _ while_AST) = validate_AST while_AST
 
 
instance Show AOp where
    show Plus  = "+"
    show Minus = "-"
 
instance Show BOp where
    show And = "&&"
    show Or  = "||"
 
instance Show AExp where
    show (AOperation exp1 op exp2) = show exp1 ++ " " ++ show op  ++ " " ++ show exp2
    show (AValue number) = show number
 
instance Show BExp where
    show (BValue boolean) = show boolean
    show (AExp boolean arithmetic) = (show boolean) ++ (show arithmetic)
    show (BOperation bool1 op bool2) = (show bool1) ++ (show op) ++ (show bool2)
 
instance Show AST where
    show (Root remaining_AST) = "(Root " ++ (show remaining_AST) ++ ")"
    show (Init params remaining_AST) = "(Init " ++ (show params) ++ (show remaining_AST) ++ ")"
    show (Asign expr1 expr2) = "(Asign " ++ show expr1 ++ " " ++ show expr2 ++ ")"
    show (If bool_expr first_branch second_branch) = "(If " ++ (show bool_expr) ++ " " ++ (show first_branch) ++ " " ++ (show second_branch) ++ ")"
    show (Instructions next_instr remaining_AST) = " " ++ (show next_instr) ++ " " ++ (show remaining_AST)
    show (While bool_expr while_AST) = "(while " ++ (show bool_expr) ++ (show while_AST) ++ ")"
 
 
--test:
--putStrLn (show (Root (Init [] (Instructions (Asign "n" (AValue 5)) (While (BValue True) (Instructions (Asign "s" (AOperation (AValue 5) Plus (AValue 2))) (Asign "s" (AOperation (AValue 1) Minus (AValue 2)))))))))
--mai astept pana sa fac operatii pe Var, maine ma ocup.