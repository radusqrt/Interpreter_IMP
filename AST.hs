import Data.Bool

data BOp = And | Or
data AOp = Plus | Minus
data AExp = Operation AExp AOp AExp | AValue Integer | AString String
data BExp = Value Bool | AExp BOp AExp | BExp BOp BExp
data AST  = Root AST | Init [String] AST | Asign String AExp | If BExp AST AST | While BExp AST | Instructions AST AST

validate_AST :: AST -> Bool
validate_AST (Root remaining_AST) = validate_AST remaining_AST
validate_AST (Init _ remaining_AST) = validate_AST remaining_AST
validate_AST (Asign _ _) = True
validate_AST (If _ first_branch second_branch) = validate_AST first_branch && validate_AST second_branch
validate_AST (Instructions next_instr remaining_AST) = validate_AST next_instr && validate_AST remaining_AST
validate_AST (While _ while_AST) = validate_AST while_AST
validate_AST _ = False

