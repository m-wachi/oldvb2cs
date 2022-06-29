module Ast

    open System

    type pos = int
    type Symbol = string

    type Var = SimpleVar of Symbol * pos

    type Expr = 
        | VarExp of Var 
        | IntExp of int
        | StringExp of string

    type Prog = Prog of Expr list


    let exprToStr expr =
        match expr with
        | IntExp n -> "Int " + n.ToString()
        | StringExp s -> "Str " + s

    let progToStr (Prog exprs) = 
        let sExprs = List.map exprToStr exprs
    
        "Prog " + String.Join(", ", sExprs)
    
    