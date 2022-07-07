module TransCs

    open System

    type Writer = IO.TextWriter

    let convAll (tOut: Writer) ast =
        //tOut.WriteLine "hello TransCs."
        tOut.Write "namespace Oldvb2Cs01\n{\n"
        tOut.Write "class Program\n{\n"
        //convLglines (os, 1, ast);
        tOut.Write "}\n}\n"

    let translate01 (parseResult: Ast.Prog) =
        let tOut = stdout
        convAll tOut parseResult

    let translate02 (parseResult: Ast.Prog) (fileName: string) =
        let tOut = new IO.StreamWriter(fileName)
        convAll tOut parseResult
        tOut.Close()
        printfn "%s output success." fileName