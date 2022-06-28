// Learn more about F# at http://fsharp.net

open System.IO
open FSharp.Text.Lexing

open Ast

let testLexerAndParserFromString text expectedCount = 
    let lexbuf = LexBuffer<char>.FromString text

    let countFromParser = Parser.start Lexer.tokenstream lexbuf

(*
    printfn "countFromParser: result = %d, expected %d" countFromParser expectedCount
*)
    printfn "%s" (Ast.progToStr countFromParser)
    
    
let testLexerAndParserFromFile (fileName:string) expectedCount = 
    use textReader = new System.IO.StreamReader(fileName)
    let lexbuf = LexBuffer<char>.FromTextReader textReader

    let countFromParser = Parser.start Lexer.tokenstream lexbuf
(*
    printfn "countFromParser: result = %d, expected %d" countFromParser expectedCount
*)
    printfn "%s" (Ast.progToStr countFromParser)
    

testLexerAndParserFromString "hello" 1
testLexerAndParserFromString "hello hello" 2

let testFile = Path.Combine(__SOURCE_DIRECTORY__, "test.txt")
File.WriteAllText(testFile, "hello hello")
testLexerAndParserFromFile testFile 2

printfn "Press any key to continue..."
System.Console.ReadLine() |> ignore


