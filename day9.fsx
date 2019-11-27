let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "/inputs/day9.txt")
            |> Seq.head

type Blah = 
    | Garbage
    | Ignore of Blah
    | Read

type State = {Level: int; Groups: int list}


"{{<a!>},{<a!>},{<a!>},{<ab>}}"
let part1 txt = 
    ((Read, {Level=1; Groups=[]}), txt) 
    ||> Seq.fold (fun (op, acc) c -> 
            match op, c with 
            | Ignore(prev), _ -> prev, acc
            | op, '!'         -> Ignore(op), acc
            | Garbage, '>'    -> Read, acc 
            | Garbage, _      -> Garbage, acc
            | Read, '<'       -> Garbage, acc
            | Read, '{'       -> Read, {acc with Level=acc.Level+1; Groups=acc.Level::acc.Groups}
            | Read, '}'       -> Read, {acc with Level=max (acc.Level-1) 1}
            | Read, _         -> Read, acc)

let _, r1 = part1 input

let part2 txt = 
    ((Read, 0), txt) 
    ||> Seq.fold (fun (op, acc) c -> 
            match op, c with 
            | Ignore(prev), _ -> prev, acc
            | op, '!'         -> Ignore(op), acc
            | Garbage, '>'    -> Read, acc 
            | Garbage, _      -> Garbage, acc + 1
            | Read, '<'       -> Garbage, acc
            | Read, _         -> Read, acc)

let _,r2 = part2 input