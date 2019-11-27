open System.Text.RegularExpressions

let lines = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "/inputs/day7.txt")

let getChildren (l : string) = l.Contains(">") |> function
    | false -> Seq.empty
    | true  -> l.Split([|'>'|]) 
                 |> Array.last
                 |> (fun cs -> cs.Split([|','|]))
                 |> Seq.map (fun c -> c.Trim())

let getNameAndWeight (l : string) =
    let m = Regex.Match(l, "(\w*) \((\d*)\).*")
    [ for g in m.Groups -> g.Value ] 
      |> List.tail
      |> fun ([n; w]) -> n, int w 

let children = lines |> Seq.collect getChildren |> Set.ofSeq
lines |> Seq.where (fun l -> children |> Set.contains (l.Split([|' '|]).[0]) |> not)

let weights = lines |> Seq.map getNameAndWeight |> Map.ofSeq

let nodes = lines |> Seq.map (fun l -> l |> getNameAndWeight, l |> getChildren)
                |> Seq.where (snd >> Seq.exists (fun _ -> true))
