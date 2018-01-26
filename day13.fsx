let parseLine (l : string) = 
    l.Split([|':'|])
      |> Array.map int
      |> fun ([|x; y|]) -> (x, y)

let scanners = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "/inputs/day13.txt")
               |> Seq.map parseLine

let catchPackets delay path = 
    let homePos range = (range - 2) * 2 + 2 
    path 
     |> Seq.choose 
       (fun (layer, range) -> (layer + delay) % (homePos range) |> function
          | 0 -> Some (layer, range)
          | _ -> None)

let part1 = scanners |> catchPackets 0 |> Seq.sumBy (fun (x,y) -> x * y)
let part2 = seq { for i in 0 .. System.Int32.MaxValue do 
                    if scanners |> catchPackets i |> Seq.isEmpty
                    then yield i } |> Seq.head  