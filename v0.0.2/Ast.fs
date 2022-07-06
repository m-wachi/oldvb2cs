module Ast

    open System

    type pos = int
    type Symbol = string
   
    type Comment = string

    type Var = SimpleVar of Symbol * pos

    type Exp = 
        | VarExp of Var 
        | IntExp of int
        | StringExp of string

    type Statement = 
        | AssignStmt of Var * Exp
        | BlankLine

    type LogicalLine = (Statement * Comment)

    type Prog = Prog of LogicalLine list


    let varToStr v =
        match v with
        | SimpleVar (sym, p) -> "SimpleVar: " + sym.ToString()
    

    let exprToStr expr =
        match expr with
        | IntExp n -> "IntExp: " + n.ToString()
        | StringExp s -> "StringExp: " + s
        | VarExp (SimpleVar (sym, pos)) -> "VarExp: SimpleVar: " + sym.ToString()

    let statementToStr stmt =
        match stmt with
        | AssignStmt (v, e) -> "AssignStmt: (" + (varToStr v) + ", " + (exprToStr e) + ")"
        | BlankLine -> "BlankLine"
    
    let logicalLineToStr (stmt, cmnt) =
        "LogicalLine: (" + (statementToStr stmt) + ", " + cmnt + ")"
        
    let progToStr (Prog lgcllns) = 
        let sLgcllns = List.map logicalLineToStr lgcllns
    
        "Prog " + String.Join(", ", sLgcllns)
    
    