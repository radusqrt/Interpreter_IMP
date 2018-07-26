{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Data.List.Split
import Data.Char
--operatiile peste tipurile de date
data AOp = Plus | Minus
data AExp = AOperation AExp AOp AExp | AValue Integer | AString String
--tipurile de date
data BExp = BCompare AExp BAOp AExp | BOperation BExp BLOp BExp | BValue Bool
data BLOp = And | Or 
data BAOp = Greater | Lesser
--AST-ul propriu zis
data AST  = Init [String] AST | Asign String AExp | If BExp AST AST | While BExp AST | Instructions AST AST | No_AST

filter_chars :: String -> String
filter_chars [] = ""
filter_chars (',':xs) = ' ' : filter_chars xs
filter_chars ('(':xs) = " ( " ++ (filter_chars xs)
filter_chars (')':xs) = " ) " ++ (filter_chars xs)
filter_chars ('{':xs) = "\n _block_start_ \n" ++ (filter_chars xs)
filter_chars ('}':xs) = "\n _block_end_ \n" ++ (filter_chars xs)
filter_chars ('=':xs) = " = " ++ (filter_chars xs)
filter_chars ('+':xs) = " + " ++ (filter_chars xs)
filter_chars (x:xs) = x : (filter_chars xs)

parse_syntax :: [[String]] -> AST
parse_syntax [] = No_AST
parse_syntax (("int":args):remaining_syntax) = (Init args (parse_syntax remaining_syntax))
parse_syntax ((assigned : "=" : asg_term):[]) = (Asign assigned (arithmetic_syntax asg_term)) --(parse_syntax remaining_syntax)) 
parse_syntax ((assigned : "=" : asg_term):remaining_syntax) = (Instructions (Asign assigned (arithmetic_syntax asg_term)) (parse_syntax remaining_syntax)) 
--parse_syntax (("if" : b_expr) : remaining_syntax) = (Instructions (If (BValue True) No_AST No_AST) (parse_syntax remaining_syntax)) 
--parse_syntax (("while" : while_expr) : remaining_syntax) = (Instructions (While (BValue True) No_AST) (parse_syntax remaining_syntax))

--boolean_syntax :: [String] -> BExp
--boolean_syntax [] = (BValue True) 
--boolean_syntax ["True"] = (BValue True)
--boolean_syntax ["False"] = (BValue False)
--boolean_syntax (x : ">" : y) = (BOperation (BCompare (arithmetic_syntax x) Greater (boolean_syntax y)) And (boolean_syntax xs))
--boolean_syntax (x : "<" : y) = (BCompare (arithmetic_syntax x) Lesser (boolean_syntax y))
--boolean_syntax (x : "&&" : y) = (BOperation (BOperation (boolean_syntax x) And (boolean_syntax y)) And (boolean_syntax xs))
--boolean_syntax (x : "||" : y) = (BOperation (BOperation (boolean_syntax x) Or (boolean_syntax y)) And (boolean_syntax xs))



arithmetic_syntax :: [String] -> AExp
arithmetic_syntax (x:[]) = (arithmetic_type x) 
arithmetic_syntax ("(" : xs) =  if remaining_syntax /= []
    then  (AOperation (arithmetic_syntax parsed_syntax) (map_arithmetic_operator (head remaining_syntax)) (arithmetic_syntax (tail remaining_syntax)))
    else (arithmetic_syntax parsed_syntax) 
        where
            (remaining_syntax, parsed_syntax) = slice_after_char (xs, []) 1 
arithmetic_syntax (x : op@"+" : xs) = (AOperation (arithmetic_type x) (map_arithmetic_operator op) (arithmetic_syntax xs))
arithmetic_syntax (x : op@"-" : xs) = (AOperation (arithmetic_type x) (map_arithmetic_operator op) (arithmetic_syntax xs))
arithmetic_syntax x = (AString (head x))

map_arithmetic_operator :: String -> AOp
map_arithmetic_operator "+" = Plus
map_arithmetic_operator "-" = Minus


slice_after_char :: ([String], [String]) -> Int -> ([String], [String])
slice_after_char (("(":xs), parsed_syntax) nr_of_parantheses = slice_after_char (xs, "(":parsed_syntax) (nr_of_parantheses + 1)
slice_after_char ((")":xs), parsed_syntax) nr_of_parantheses = if nr_of_parantheses == 1
    then (xs, (reverse parsed_syntax))
    else (slice_after_char (xs, ")":parsed_syntax) (nr_of_parantheses - 1))    
slice_after_char (x:xs, parsed_syntax) nr_of_parantheses   = slice_after_char (xs, x:parsed_syntax) nr_of_parantheses 


arithmetic_type :: String -> AExp
arithmetic_type str = if (validate_number str)
    then (AValue (read str :: Integer))
    else (AString str)

validate_number :: String -> Bool
validate_number [] = True
validate_number (x:xs) = if (isNumber x)
    then validate_number xs
    else False

tokenize :: String -> [[String]]
tokenize str = (filter (/= []) (map (filter (/= "")) (map (splitOn " ") (splitOn "\n" str))))


main = do putStrLn (show new_AST)
    where
        new_AST = parse_syntax tokens
        tokens = tokenize parsed
        parsed = (filter_chars unparsed_cod)
        unparsed_cod = 
            "int s, n \n \
            \ n = (1000 + (5 + 7)) \n \
            \ s = (n + (3 + (2 + 7) - n + (s - 3)))\n"


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
    show (BOperation bool1 op bool2) = (show bool1) ++ (show op) ++ (show bool2)
 
instance Show AST where
    show (Init params remaining_AST) = "(Init " ++ (show params) ++ (show remaining_AST) ++ ")"
    show (Asign expr1 expr2) = "(Asign " ++ show expr1 ++ " " ++ show expr2 ++ ")"
    show (If bool_expr first_branch second_branch) = "(If " ++ (show bool_expr) ++ " " ++ (show first_branch) ++ " " ++ (show second_branch) ++ ")"
    show (Instructions next_instr remaining_AST) = "(Instructions" ++ (show next_instr) ++ " " ++ (show remaining_AST) ++ ")"
    show (While bool_expr while_AST) = "(while " ++ (show bool_expr) ++ (show while_AST) ++ ")"
 
 


