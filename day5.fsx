let maze = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "/inputs/day5.txt")
            |> Array.map int

type Direction = int -> int
type Jumper = int -> int

let part1Direction = ((+)1)
let part2Direction = function
    | y when y > 2 -> y - 1
    | y            -> y + 1

let jump (direction : Direction) (maze : int []) index =
     let x = maze.[index]
     maze.[index] <- x |> direction
     index + x

let folder (jmp : Jumper) exit = function
    | x when x >= exit -> None
    | x                -> Some (x, jmp x)

let calculate (d : Direction) m =
    let j = m |> Array.copy |> jump d
    0 |> Seq.unfold (folder j (m.Length))
      |> Seq.length

let part1 = calculate part1Direction 
let part2 = calculate part2Direction  