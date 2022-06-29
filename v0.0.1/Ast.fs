module Ast

    open System

    type pos = int
    type Symbol = string

    type Var = SimpleVar of Symbol * pos

    type Exp = 
        | VarExp of Var 
        | IntExp of int
        | StringExp of string

    type Prog = Prog of Exp list


    let exprToStr expr =
        match expr with
        | IntExp n -> "IntExp: " + n.ToString()
        | StringExp s -> "StringExp: " + s
        | VarExp (SimpleVar (sym, pos)) -> "VarExp: SimpleVar: " + sym.ToString()
        
    let progToStr (Prog exprs) = 
        let sExprs = List.map exprToStr exprs
    
        "Prog " + String.Join(", ", sExprs)
    
    