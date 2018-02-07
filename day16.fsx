#r "packages/FParsec/lib/net40-client/FParsecCS.dll"
#r "packages/FParsec/lib/net40-client/FParsec.dll"
open FParsec

type Operation =
    | Spin of int
    | Exchange of int * int
    | Partner of char * char

type ParserState = unit
let pspin : Parser<Operation,ParserState> = pchar 's' >>. pint32 |>> Spin

let pexchange = pchar 'x' >>. tuple2 (pint32 .>> pchar '/') (pint32) |>> Exchange
let ppartner = pchar 'p' >>. tuple2 (asciiLetter .>> pchar '/') (asciiLetter) |>> Partner

let parseOperation = pspin <|> pexchange <|> ppartner

let parseOperations = run (sepBy parseOperation (pstring ","))
let sample = "po/k,x4/0,s12,x7/6"
//let pexchange = pchar 'x' >>. tuple2 

let spin x (programs : char []) =
    let index = programs.Length - x
    programs.[0..index-1] |> Array.append (programs.[index..])

let exchange x y (programs : char []) =
    let ps = Array.copy programs
    let p = ps.[x]
    ps.[x] <- ps.[y]
    ps.[y] <- p
    ps

let partner p1 p2 (programs : char []) =
    let i1 = programs |> Array.findIndex ((=) p1)
    let i2 = programs |> Array.findIndex ((=) p2)
    exchange i1 i2 programs

let (|RegexMatch|_|) pattern input =
    let m = System.Text.RegularExpressions.Regex.Match(input, pattern)
    if m.Success
    then [ for g in m.Groups -> g.Value ] |> List.tail |> Some
    else None


let parseOperation' operation = operation |> function
    | RegexMatch "(\w)(\d*)" ["s"; n]    -> Spin (n |> int)
    | RegexMatch "x(\d*)/(\d*)" [n1; n2] -> Exchange ((n1 |> int), (n2 |> int))
    | RegexMatch "p(\w)/(\w)" [p1; p2]   -> Partner ((p1.[0]), (p2.[0]))
    | _                                  -> operation |> sprintf "Unknown: %s" |> failwith 

let operations = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + "/inputs/day16.txt").Split([|','|])
                 |> Seq.map parseOperation'
                 |> Seq.toArray


let apply x f = f x

let operation = function
    | Spin x         -> spin x
    | Exchange (x,y) -> exchange x y
    | Partner (x,y)  -> partner x y
    

let execute programs = 
    Seq.fold (fun item op -> op |> operation |> apply item) programs
  
let part1 = execute [|'a' .. 'p'|] operations |> System.String

let findCycle programs ops =
    let hash = System.Collections.Generic.HashSet<string> ()
    let rec loop (ps : char []) x =
        seq { if ps |> System.String |> hash.Add |> not
              then yield x

              let ps' = execute ps ops 
              yield! loop ps' (x+1)
        }
    loop programs 0


let iterate ops programs = 
    let f state = execute (state |> Seq.toArray) ops
    Seq.unfold  (f >> (fun r -> Some (r, r))) (programs)

let part2Cycle ops programs = 
    let cycle = findCycle programs ops |> Seq.head
    let rem = 1000000000 % cycle
    programs |> iterate operations |> Seq.skip (rem - 1) |> Seq.head |> System.String


let part2Memoized ops programs = 
    let dict = System.Collections.Generic.Dictionary<string,string> ()
    let f state = if state |> dict.ContainsKey  
                  then dict.[state]
                  else
                      let r = execute (state |> Seq.toArray) ops |> System.String
                      dict.[state] <- r
                      r
    Seq.unfold  (f >> (fun r -> Some (r, r))) (programs)

let part2MemoizedCycle ops (programs : System.String) =
    let dict = System.Collections.Generic.Dictionary<string,string> ()
    
    let rec loop ps x =
        seq { if ps |> dict.ContainsKey
              then yield x

              let ps' = execute (ps |> Seq.toArray) ops |> System.String
              dict.[ps] <- ps'
              yield! loop ps' (x+1)
        }
        
    let cycle = loop programs 0 |> Seq.head
    let rem = 1000000000 % cycle

    let f state = if state |> dict.ContainsKey  
                  then dict.[state]
                  else
                      let r = execute (state |> Seq.toArray) ops |> System.String
                      dict.[state] <- r
                      r

    Seq.unfold  (f >> (fun r -> Some (r, r))) (programs)
      |> Seq.skip (rem - 1) 
      |> Seq.head

#time
let part2 = [|'a' .. 'p'|] |> part2Cycle operations
let part2' = [|'a' .. 'p'|] |> System.String |> part2MemoizedCycle operations