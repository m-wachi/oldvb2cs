// Learn more about F# at http://fsharp.net

open System.IO
open FSharp.Text.Lexing

let parseFile (fileName: string) =
    use textReader = new System.IO.StreamReader(fileName)
    let lexbuf = LexBuffer<char>.FromTextReader textReader

    Parser.start Lexer.tokenstream lexbuf
    

let testFile = Path.Combine(__SOURCE_DIRECTORY__, "test.txt")
let prsRes = parseFile testFile

printfn "%s" (Ast.progToStr prsRes)



