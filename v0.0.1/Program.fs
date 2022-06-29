// Learn more about F# at http://fsharp.net

open System.IO
open FSharp.Text.Lexing


let parseFile (fileName: string) =
    use textReader = new System.IO.StreamReader(fileName)
    let lexbuf = LexBuffer<char>.FromTextReader textReader

    Parser.start Lexer.tokenstream lexbuf
    

[<EntryPoint>]
let main argv =

    let testFile = Path.Combine(__SOURCE_DIRECTORY__, argv.[0])
    let prsRes = parseFile testFile

    printfn "%s" (Ast.progToStr prsRes)

    0 // return an integer exit code

