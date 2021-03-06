module TransCs

    open System

    type Writer = IO.TextWriter

    let outputWithIndent (tOut: Writer) (idt:int) (s: string) =
        let AN_INDENT = "    "
        let sIndent = String.replicate idt AN_INDENT
        tOut.Write (sIndent + s)

    let convProcName (procName: string) : string =
        match procName with
            | "Msgbox" -> "Console.WriteLine"
            | _ -> procName
            
    let convSym (sym: Ast.Symbol) = sym

    let convVbPrimType (t: Ast.VbPrimType) =
         match t with
         | Ast.VbTyString -> "string"
         | Ast.VbTyInt -> "int"

    let convVbType (t: Ast.VbType) =
        match t with
            | Ast.VbTySimple pt -> convVbPrimType pt
            | Ast.VbTyArray pt -> (convVbPrimType pt) + "[]" 

    let convProcParamDec (prm: Ast.Field) =
        let sType = convVbType prm.Vbty
        let sParamName = convSym (prm.Name)
        sType + " " + sParamName
       
    let convVar (v: Ast.Var) =
        match v with
            | Ast.SimpleVar (sym, _) -> convSym sym

    let convOper (oper: Ast.Oper) =
        match oper with
            | Ast.EqOp -> "="
            | Ast.NeqOp -> "!="
            | Ast.AmpOp -> "+"

    let rec convExp (e: Ast.Exp) =
        match e with
            | Ast.VarExp v -> convVar v
            | Ast.IntExp i -> i.ToString()
            | Ast.StringExp s -> "\"" + s + "\""
            | Ast.OpExp (lft, oper, rgt, p) -> convOpExp lft oper rgt p
            
    and convOpExp lft (oper: Ast.Oper) rgt p = 
        let sLeft = convExp lft
        let sOper = convOper oper
        let sRight = convExp rgt
        sLeft + " " + sOper + " " + sRight
            
            
    let convProcParams (prms: Ast.Exp list) =
          String.Join(", ", (List.map convExp prms))


    let rec convStmt (tOut: Writer) (idt:int) (stmt: Ast.Statement) =
        match stmt with
            | Ast.LclVarDecl (v, t) -> convLclVarDecl tOut idt v t
            | Ast.ProcDec (nm, pms, bdy, p) -> convProcDec tOut idt nm pms bdy p
            | Ast.BlankLine -> outputWithIndent tOut idt ""
            | Ast.AssignStmt (v, e) -> 
                let sStmt = (convVar v) + " = " + (convExp e) + ";"
                outputWithIndent tOut idt sStmt
            | Ast.CallProc (sym, prms) ->
                  let procName = convProcName (convSym sym)
                  let sProcStmt = procName + "(" + (convProcParams prms) + ");"
                  outputWithIndent tOut idt sProcStmt
            //| _ -> tOut.WriteLine "(not implemented yet...)"
            
    and convLclVarDecl (tOut: Writer) (idt:int) v t =
        let sStmt = (convVbType t) + " " + (convVar v) + ";"
        outputWithIndent tOut idt sStmt
        
    and convProcDec (tOut: Writer) (idt:int) nm pms bdy p = 
        let procName = nm
        let sParam = String.Join(", ", (List.map convProcParamDec pms))
        let procHdr = "static void " + procName + "(" + sParam + ")\n" 
        outputWithIndent tOut idt procHdr
        outputWithIndent tOut idt "{\n"
        convLglines tOut (idt+1) bdy
        outputWithIndent tOut idt "}\n"

    and convLgline (tOut: Writer) (idt:int) ((stmt: Ast.Statement), (cmnt: Ast.Comment)) =
        let s = if "" <> cmnt then ("//" + cmnt) else ""
        convStmt tOut idt stmt
        tOut.Write (s + "\n")
        
    and convLglines (tOut: Writer) (idt:int) (lines: Ast.LogicalLine list) =
        match lines with
        | [] -> ()
        | ln::lns -> 
            convLgline tOut idt ln
            convLglines tOut idt lns

    let convAll (tOut: Writer) (Ast.Prog lglines) =
        //tOut.WriteLine "hello TransCs."
        tOut.Write "namespace Oldvb2Cs01\n{\n"
        tOut.Write "class Program\n{\n"
        convLglines tOut 1 lglines
        tOut.Write "}\n}\n"

    let translate01 (parseResult: Ast.Prog) =
        let tOut = stdout
        convAll tOut parseResult

    let translate02 (parseResult: Ast.Prog) (fileName: string) =
        let tOut = new IO.StreamWriter(fileName)
        convAll tOut parseResult
        tOut.Close()
        printfn "%s output success." fileName
        
