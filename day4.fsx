open System.IO

let inputFile = __SOURCE_DIRECTORY__ + "/inputs/day4.txt"

let part1 file = 
    file |> File.ReadAllLines
      |> Array.where (fun l -> 
            let hash = System.Collections.Generic.HashSet<string> ()
            l.Split ' ' 
                |> Array.forall (hash.Add))
      |> (fun pass -> pass.Length)

let part2 file = 
    file |> File.ReadAllLines
      |> Array.where (fun l -> 
            let hash = System.Collections.Generic.HashSet<string> ()
            l.Split ' ' 
                |> Array.map (Seq.sort >> Seq.toArray >> (fun pass -> System.String(pass)))
                |> Array.forall (hash.Add))
      |> (fun pass -> pass.Length)
