let maze = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "/inputs/day5.txt")
            |> Array.map int

type Stepper = int -> int
type State = int * int []
type Jumper = State -> State

let part2Inc = function
    | y when y > 2 -> y - 1
    | y            -> y + 1

let jumper (stepper : Stepper) (i,m) =
    let m' = Array.copy m
    let x = m.[i]
    m'.[i] <- x |> stepper
    i + x, m'

let execute (jump : Jumper) (maze : int []) : int =
    (0, maze) |> Seq.unfold (function 
      | x, m when x >= m.Length -> None
      | s                       -> Some (s, jump s))
    |> Seq.length

let part1 = execute (jumper ((+) 1))
let part2 = execute (jumper part2Inc)

maze |> part1
// Real: 00:00:00.388, CPU: 00:00:00.388, GC gen0: 322, gen1: 0  

maze |> part2
// Real: 00:00:26.553, CPU: 00:00:26.934, GC gen0: 23031, gen1: 10