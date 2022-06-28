module Ast

type Expr = 
    | Int of int
    | Str of string


type Prog = Prog of Expr list

let exprToStr expr =
    match expr with
    | Int n -> "Int " + n.ToString()
    | Str s -> "Str " + s

let progToStr (Prog exprs) = 
    "Prog " + (exprToStr exprs.Head)
    
    