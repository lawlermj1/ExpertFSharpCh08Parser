// Polynomial.fs 

module Polynomial 

//   tokenizer 
type Token = 
    | ID of string 
    | INT of int
    | HAT 
    | PLUS 
    | MINUS 

let regex s = System.Text.RegularExpressions.Regex(s) 
let tokenR = regex @"((?<token>(\d+|\w+|\^|\+|\-))\s*)*"

let tokenize (s : string) =
    [for x in tokenR.Match(s).Groups.["token"].Captures do
        let token = 
            match x.Value with 
            | "^" -> HAT 
            | "-" -> MINUS 
            | "+" -> PLUS
            | s when System.Char.IsDigit s.[0] -> INT (int s)
            | s -> ID s 
        yield token ]


//  recursive descent parsing 
type Term = 
    | Term of int * string * int 
    | Const of int 

// "x^5 - 2x^3 + 20" = ex 
// let exTL = [Term (1,"x",5); Term(-2,"x",3); Const 20]

type Polynomial = Term List 
type TokenStream = Token List 

let tryToken (src : TokenStream) = 
    match src with 
    | tok :: rest -> Some(tok, rest)
    | _ -> None 

let parseIndex src = 
    match tryToken src with 
    | Some (HAT, src) -> 
        match tryToken src with 
        | Some (INT num2, src) -> num2, src 
        | _ -> failwith "expected an integer after '^'" 
    | _ -> 1, src 

let parseTerm src = 
    match tryToken src with 
    | Some (INT num, src) -> 
        match tryToken src with 
        | Some (ID id, src) -> 
            let idx, src = parseIndex src 
            Term (num, id, idx), src 
        | _ -> Const num, src 
    | Some (ID id, src) -> 
        let idx, src = parseIndex src 
        Term (1, id, idx), src 
    | _ -> failwith "end of token stream in term"  

// minus does not work 
let rec parsePolynomial src = 
    let t1, src = parseTerm src 
    match tryToken src with 
    | Some (PLUS, src) -> 
        let p2, src = parsePolynomial src 
        (t1 :: p2), src 
    | Some (MINUS, src) -> 
        let m2, src = parsePolynomial src 
        (t1 :: m2), src 
    | _-> [t1], src 

let parse input = 
    let src = tokenize input 
    let result, src = parsePolynomial src
    match tryToken src with 
    | Some _ -> failwith "unexpected input at end of token stream!"  
    | None -> result 

