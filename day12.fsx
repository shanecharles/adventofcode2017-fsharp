open System.Collections.Generic
let input = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "/inputs/day12.txt")
            |> Array.map (fun l -> 
                 let pipes = l.Split("<->")
                 int pipes.[0], pipes.[1].Split([|','|]) |> Array.map (int))
            |> dict

let getEdges (lookup: IDictionary<int,int[]>) origin = 
    let hash = new System.Collections.Generic.HashSet<int>([origin])
    [|origin|] 
    |> Seq.unfold (fun ns ->  
            match ns |> Array.collect (fun n -> lookup.[n])  |> Array.filter (hash.Add) with 
            | [||] -> None
            | n'   -> Some (n', n'))
    |> Seq.toList
    |> ignore
    hash

let part1 = (getEdges input 0).Count

let part2 = 
    ([], input.Keys)
    ||> Seq.fold (fun acc k -> 
            if acc |> List.exists (fun (h: System.Collections.Generic.HashSet<int>) -> h.Contains k) 
            then acc 
            else (getEdges input k) :: acc)
    |> Seq.length