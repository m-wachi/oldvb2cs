// Learn more about F# at http://fsharp.net

open System.IO
open FSharp.Text.Lexing


let parseFile (fileName: string) =
    use textReader = new System.IO.StreamReader(fileName)
    let lexbuf = LexBuffer<char>.FromTextReader textReader
    try
        Parser.prog Lexer.sts_initial lexbuf
    with
        | ex -> 
            printfn "ERROR!!:\n"
            printfn "%A" (ex.ToString())
            raise ex

[<EntryPoint>]
let main argv =

    let cmd = argv.[0]
    let testFile = Path.Combine(__SOURCE_DIRECTORY__, argv.[1])
    let prsRes = parseFile testFile

    match cmd with
    | "parse" -> printfn "%s" (Ast.progToStr prsRes)
    | "transcs01" -> TransCs.translate01 prsRes
    | "transcs02" ->
        let outFile = Path.Combine(__SOURCE_DIRECTORY__, argv[2])
        TransCs.translate02 prsRes outFile
    | _ -> printfn "unknown command.."

    0 // return an integer exit code

