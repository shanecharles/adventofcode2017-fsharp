let spin x (programs : char []) =
    let index = programs.Length - x
    programs.[0..index-1] |> Array.append (programs.[index..])

let exchange x y (programs : char []) =
    //let ps = Array.copy programs
    let ps = programs
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


let parseOperation operation = operation |> function
    | RegexMatch "(\w)(\d*)" ["s"; n]            -> spin (n |> int)
    | RegexMatch "(\w)(\d*)/(\d*)" ["x"; n1; n2] -> exchange (n1 |> int) (n2 |> int)
    | RegexMatch "(\w)(\w)/(\w)" ["p"; p1; p2]   -> partner (p1.[0]) (p2.[0])
    | _                                          -> operation |> sprintf "Unknown: %s" |> failwith 

let operations = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + "/inputs/day16.txt").Split([|','|])
                 |> Seq.map parseOperation
                 |> Seq.toArray

let execute programs = 
    operations |> Seq.fold (fun item f -> f item) programs
  
let part1 = execute [|'a' .. 'p'|] |> System.String
"abcdefghijklmnop"
"giadhmkpcnbfjelo"

let mapMutations (ouput : char []) = Array.map (fun c -> ouput |> Array.findIndex ((=)c))
let shortcut mapping input = 
    let output = Array.copy input
    mapping 
        |> Array.iteri (fun i oi -> output.[oi] <- input.[i]) 
    output

[|'a' .. 'p'|]
  |> exchange 0 2
  |> exchange 1 10
  |> exchange 2 8
  |> System.String


let part2 = 
    let mapping = [|'a' .. 'p'|] |> mapMutations (part1 |> Seq.toArray)
    let short = shortcut mapping
    Seq.fold (fun acc _ -> short acc) [|'a'..'p'|] {1 .. 40}
      |> System.String