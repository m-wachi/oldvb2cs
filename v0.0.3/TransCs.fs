module TransCs

    open System

    type Writer = IO.TextWriter

    let outputWithIndent (tOut: Writer) (idt:int) (s: string) =
        let AN_INDENT = "    "
        let sIndent = String.replicate idt AN_INDENT
        tOut.Write (sIndent + s)


    let rec convStmt (tOut: Writer) (idt:int) (stmt: Ast.Statement) =
        match stmt with
            // Absyn.LclVarDecl (v, t) => convLclVarDecl (os, idt, (v, t))
            //| Absyn.ProcDec r => convProcDec (os, idt, r)
            | Ast.ProcDec (nm, pms, bdy, p) -> convProcDec tOut idt nm pms bdy p
            //| Absyn.BlankLine => (outputWithIndent (os, idt, "");"")
            | Ast.BlankLine -> outputWithIndent tOut idt ""
            //| Absyn.AssignStmt (v, e) => 
            //    let
            //        (* val sIdt = MwUtil.repeatStr AN_INDENT idt *)
            //        val s = (convVar v) ^ " = " ^ (convExp e) ^ ";"
            //    in
            //        (* TextIO.output (os, sIdt ^ s);  *)
            //        outputWithIndent (os, idt, s);
            //        ""
            //    end
            //| Absyn.CallProc (sym, params) =>
            //    let
            //        val procName = convProcName (convSym sym)
            //        val sProcStmt = procName ^ "(" ^ (convProcParams params) ^ ");"
            //    in
            //         (* TextIO.output (os, sProcStmt); *)
            //         outputWithIndent (os, idt, sProcStmt);
            //         ""
            //    end
            | _ -> tOut.WriteLine "(not implemented yet...)"
            
    and convProcDec (tOut: Writer) (idt:int) nm pms bdy p = 
        //let
        //    val procName = convSym (#name r)
        //    val sParam = MwUtil.strJoin (", ", (map convProcParamDec (#params r)))
        //    val procHdr = "static void " ^ procName ^ "(" ^ sParam ^ ")\n" 
        let procName = nm
        let sParam = ""    //not implemented yet.
        let procHdr = "static void " + procName + "(" + sParam + ")\n" 
        //in
        //    outputWithIndent (os, idt, procHdr);
        outputWithIndent tOut idt procHdr
        //    outputWithIndent (os, idt, "{\n");
        //    convLglines (os, idt+1, (#body r));
        //    (* TextIO.output (os, (sIdt ^ "}\n")); *)
        //    outputWithIndent (os, idt, "}\n");
        //    ""
        //end

    and convLgline (tOut: Writer) (idt:int) ((stmt: Ast.Statement), (cmnt: Ast.Comment)) =
        let s = if "" <> cmnt then ("//" + cmnt) else ""
        //convStmt (os, idt, stmt);
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
        
