// Learn more about F# at http://fsharp.org

open Polynomial 

[<EntryPoint>]
let main argv =

    let exString = @"x^5 - 2x^3 + 20"
    printfn "Example tokenize x^5 - 2x^3 + 20 = %A " (tokenize exString) 
    printfn " "  

    printfn "Example parse 1+3 = %A " (parse "1+3") 
    printfn " "  

    printfn "Example parse 2x^2+3x+5 = %A " (parse "2x^2+3x+5") 
    printfn " "  

    printfn "Example parse x^5 - 2x^3 + 20 = %A " (parse exString) 
    printfn " "  

    printfn " "  
    printfn "All finished from ExpertF#Ch08Parser" 
    0 // return an integer exit code
