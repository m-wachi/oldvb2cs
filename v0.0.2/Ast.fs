module Ast

    open System

    type pos = int
    type Symbol = string
   
    type Comment = string

    type Var = SimpleVar of Symbol * pos

    type VbPrimType = VbTyString | VbTyInt

    type VbType = VbTySimple of VbPrimType  | VbTyArray of VbPrimType

    type Field = {Name: Symbol; Vbty: VbType; Pos: pos}

    type Exp = 
        | VarExp of Var 
        | IntExp of int
        | StringExp of string

    type Statement = 
        | AssignStmt of Var * Exp
        | BlankLine
        | ProcDec of 
            name: Symbol * prmtrs: Field list * body: LgLine list * pos: pos
            
    and LgLine = (Statement * Comment)

    type LogicalLine = (Statement * Comment)

    type Prog = Prog of LogicalLine list

    let vbprimtypeToStr (t: VbPrimType) : string =
        match t with 
        | VbTyString -> "String"
        | VbTyInt -> "Integer"

    let vbtypeToStr (t: VbType) : string =
        match t with
            VbTySimple pt -> vbprimtypeToStr pt
            | VbTyArray pt -> "Array of " + (vbprimtypeToStr pt)

    let varToStr v =
        match v with
        | SimpleVar (sym, p) -> "SimpleVar: " + sym.ToString()
    
    let fieldToStr (fld: Field) =
        fld.Name + ":" + (vbtypeToStr fld.Vbty)
   

    let exprToStr expr =
        match expr with
        | IntExp n -> "IntExp: " + n.ToString()
        | StringExp s -> "StringExp: \"" + s + "\""
        | VarExp (SimpleVar (sym, pos)) -> "VarExp: SimpleVar: " + sym.ToString()

    let rec statementToStr stmt =
        match stmt with
        | AssignStmt (v, e) -> "AssignStmt: (" + (varToStr v) + ", " + (exprToStr e) + ")"
        | BlankLine -> "BlankLine"
        | ProcDec (nm, pms, bdy, p) -> (procDecToStr nm pms bdy p)

    and procDecToStr nm pms bdy p =
        let procName = nm
        let sParam = String.Join(", ", (List.map fieldToStr pms))
        let procHdr = "ProcDec " + procName + "(" + sParam + ")\n" 
        let body = logicalLinesToStr bdy
        procHdr + body + "End ProcDec " + procName

    and logicalLineToStr (stmt, cmnt) =
        "LogicalLine: (" + (statementToStr stmt) + ", " + cmnt + ")"

    and logicalLinesToStr (lglines: LgLine list) =
        match lglines with
        | [] -> ""    //empty case
        | lgl1::lgls -> (logicalLineToStr lgl1) + (logicalLinesToStr lgls)
        
    let progToStr (Prog lgcllns) = 
        let sLgcllns = List.map logicalLineToStr lgcllns
    
        "Prog " + String.Join(", ", sLgcllns)
    
    