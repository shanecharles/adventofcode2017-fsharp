
let input = System.IO.File.ReadAllText(__SOURCE_DIRECTORY__ + "/inputs/day10.txt")

let lengths = input |> function l -> l.Split([|','|]) |> Array.map int

let suffix = [|17; 31; 73; 47; 23|]
let ascii = System.Text.Encoding.ASCII.GetBytes(input) 
            |> Array.map int
            |> (fun ar -> Array.concat [ar; suffix])


System.Text.Encoding.ASCII.GetBytes("1,2,3") |> Array.map int
let aoc2017 = System.Text.Encoding.ASCII.GetBytes("AoC 2017") 
              |> Array.map int
              |> (fun ar -> Array.concat [ar; suffix])

type State = { Position: int; Hash: int []; SkipSize: int }

let getIndexes arrayLength currentIndex length = 
    [currentIndex .. (currentIndex + length - 1)] 
        |> Seq.map (fun x -> x % arrayLength)
        |> Seq.toArray

let processState (state: State) len =
    let indexes = getIndexes state.Hash.Length state.Position len
    let segment = indexes |> Array.map (fun x -> state.Hash.[x]) |> Array.rev
    segment |> Array.iteri (fun i x -> state.Hash.[indexes.[i]] <- x) |> ignore
    {state with SkipSize=state.SkipSize+1; Position=(state.Position+len+state.SkipSize)%state.Hash.Length}

let initState () = {Position=0; Hash=[|0 .. 255|]; SkipSize=0}


let part1 = (initState (), lengths)
            ||> Seq.fold (fun s l -> processState s l)
            |> (fun state -> state.Hash |> Seq.take 2 |> Seq.reduce (*))


let part2 = System.BitConverter.ToString(
                (initState (), seq { for i in [1 .. 64] do yield! ascii })
                ||> Seq.fold (fun s l -> processState s l)
                |> (fun state -> state.Hash |> Array.chunkBySize 16)
                |> Array.map (Array.reduce (^^^))
                |> Array.map byte).Replace("-","").ToLower() 
