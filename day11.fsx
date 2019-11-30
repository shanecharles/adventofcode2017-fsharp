let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + "/inputs/day11.txt").Split([|','|])

type Position = (int * int * int)
let move (x,y,z) = function
    | "n"  -> (x, y+1,z-1)
    | "s"  -> (x, y-1,z+1)
    | "ne" -> (x+1, y, z-1)
    | "se" -> (x+1, y, z+1)
    | "nw" -> (x-1, y+1, z)
    | "sw" -> (x-1, y-1, z)
let tiles = ((0,0,0), input) ||> Array.scan move
let furthest (inp: Position seq) = 
    (0, inp) ||> Seq.fold (fun o (x,y,z) -> o :: ([x;y;z] |> List.map (System.Math.Abs)) |> List.max)

let countSteps (x,y,z) =
    [x;y;z] |> List.map (fun (c: int) -> System.Math.Abs c) |> List.max

let part1 = tiles |> Array.last |> countSteps
let part2 = tiles |> furthest
