#r "packages/FParsec/lib/net40-client/FParsecCS.dll"
#r "packages/FParsec/lib/net40-client/FParsec.dll"
open FParsec

let parseLine : Parser<int * int, unit> = tuple2 (pint32 .>> pstring ": ") pint32 

let scanners = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "/inputs/day13.txt")
               |> Seq.map (run parseLine >> (function Success (x,_,_) -> x))

let catchPackets delay path = 
    let homePos range = (range - 1) * 2
    path 
     |> Seq.choose 
       (fun (layer, range) -> (layer + delay) % (homePos range) |> function
          | 0 -> Some (layer, range)
          | _ -> None)

let uncurry f (x, y) = f x y

let part1 = scanners |> catchPackets 0 |> Seq.sumBy (uncurry (*))

let part2 = seq { for i in 0 .. System.Int32.MaxValue do 
                    if scanners |> catchPackets i |> Seq.isEmpty
                    then yield i } 
            |> Seq.head