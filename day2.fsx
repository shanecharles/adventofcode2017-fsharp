open System.Runtime.InteropServices
let inputFile = __SOURCE_DIRECTORY__ + "/inputs/day2.txt"

let spreadsheet = System.IO.File.ReadAllLines inputFile
                  |> Array.map 
                    (fun l -> l.Split([|'\t'|]) 
                                |> Seq.choose (fun v -> match System.Int32.TryParse(v) with
                                                        | (true, v') -> Some v'
                                                        | _          -> None)
                                |> Seq.toList)

let part1 spread = 
    spread
      |> Seq.sumBy (fun row -> (row |> List.max) - (row |> List.min))

let findDivDiff spreadline =
    let rec loop xs =
        seq {
            match xs with
            | [] | [_] -> ()
            | x :: rm  -> 
              yield! Seq.choose (fun y -> match y % x with 
                                          | 0 -> Some (y / x)
                                          | _ -> None) rm
              yield! loop rm
        }
    spreadline |> List.sort |> loop |> Seq.head

let part2 spread = 
    spread |> Seq.sumBy findDivDiff

let result1 = spreadsheet |> part1
let result2 = spreadsheet |> part2