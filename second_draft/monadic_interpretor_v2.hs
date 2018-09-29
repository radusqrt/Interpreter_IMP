import Data.Map as M
-- o config
data AOp = Plus | Minus
data AExp = AOperation AExp AOp AExp | AValue Integer | AString String
--tipurile de date
data BExp = BCompare AExp BAOp AExp | BOperation BExp BLOp BExp | BValue Bool
data BLOp = And | Or
data BAOp = Greater | Lesser
--AST-ul propriu zis
data AST  = Init [String] AST | Asign String AExp | If BExp AST AST | While BExp AST | Instructions AST AST | No_AST

data Item = Item_AST AST 
        | Item_BAOp BAOp 
        | Item_BLOp BLOp 
        | Item_AExp AExp 
        | Item_BExp BExp
        | IfC AST AST 
        | OpR AExp
        | OpL AExp
        | BOpR BExp
        | BOpL BExp
        | Aritm AOp
        | BArim BAOp
        | BLog BLOp
        | Var String

type Cfg = ([Item], Map String Integer)


interpretor :: Cfg -> Cfg

--am facut doar pentru Plus, dar pot generaliza pentru oricare operatie (pun pe stiva si functia)
interpretor ((Item_AExp (AOperation (AValue v) Plus (AValue v'))):xs,m) = ((Item_AExp (AValue (v+v'))):xs,m)
interpretor ((Item_AExp (AOperation e op e')):xs,m) = ((Item_AExp e):(OpR e'):(Aritm op):xs,m)
interpretor ((Item_AExp (AValue v)):(OpR e'):(Aritm op):xs,m) = ((Item_AExp e'):(OpL (AValue v)):(Aritm op):xs,m)
interpretor ((Item_AExp (AValue v')):(OpL v):(Aritm op):xs,m) = (((Item_AExp (AOperation v op (AValue v'))):xs),m)
interpretor ((Item_AExp (AString str)):xs,m) = (((Item_AExp (AValue (m M.! str))):xs),m)
--interpretor ((Item_AExp  e):xs,m) = (xs, m)

--am facut doar pentru AND/Greater, dar pot generaliza pentru orice operator prin stiva
interpretor ((Item_BExp (BOperation (BValue v1) And (BValue v2))):xs, m) = ((Item_BExp (BValue (v1 && v2))):xs,m)
interpretor ((Item_BExp (BOperation e1 And e2)):xs, m) = ((Item_BExp e1):(BOpR e2):xs, m)
interpretor ((Item_BExp (BValue v1)):(BOpR e2):xs, m)  = ((Item_BExp e2):(BOpL (BValue v1)):xs, m)
interpretor ((Item_BExp (BValue v2)):(BOpL v1):xs, m)  = ((Item_BExp (BOperation v1 And (BValue v2))):xs, m)

-- --pun op-ul pe stiva, model pentru cele de mai sus
interpretor ((Item_BExp (BCompare (AValue v1) Greater (AValue v2))):xs, m) = ((Item_BExp (BValue (v1 > v2))):xs,m)
interpretor ((Item_BExp (BCompare e1 op e2)):xs, m) = ((Item_AExp e1):(OpR e2):(BArim op):xs,m)
interpretor ((Item_AExp (AValue v1)):(OpR e2):(BArim op):xs, m) = ((Item_AExp e2):(OpL (AValue v1)):(BArim op):xs,m)
interpretor ((Item_AExp (AValue v2)):(OpL v1):(BArim op):xs, m)  = ((Item_BExp (BCompare v1 op (AValue v2))):xs, m)

interpretor ((Item_AST (Init str ast)):xs, m)   = ((Item_AST ast):xs, (set_values str m))
interpretor ((Item_AST (Asign str aexp)):xs, m) = ((Item_AExp aexp):(Var str):xs,m)

interpretor (((Item_AST (If (BValue True) p p')):xs),m)  = ((Item_AST p):xs,m)
interpretor (((Item_AST (If (BValue False) p p')):xs),m) = ((Item_AST p'):xs,m)
interpretor (((Item_AST (If c p p')):xs),m)              = ((Item_BExp c):(IfC p p'):xs,m)             
interpretor (((Item_BExp (BValue b)):(IfC p p'):xs),m)   = ((Item_AST (If (BValue b) p p)):xs,m)

interpretor ((Item_AST (Instructions ast1 ast2)):xs, m)  = ((Item_AST ast1):(Item_AST ast2):xs, m)
interpretor ((Item_AST (While bexpr ast)):xs, m) = ((Item_AST ast):(Item_AST (While bexpr ast)):xs, m)
interpretor ((Item_AST (No_AST)):xs, m) = (xs, m)
interpretor ([], m) = ([], m)


set_values :: [String] -> M.Map String Integer -> M.Map String Integer
set_values (x:xs) map = (set_values xs (M.insert x 0 map))
set_values [] map = map 
