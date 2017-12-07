let input = 289326

let rings = Seq.initInfinite (fun i -> (i, pown (i * 2 + 1) 2))

let part1 location = 
    let rowOffset ringId limit =
            match (location - limit) % (ringId * 2) with
            | 0                     -> ringId 
            | x when x - ringId < 0 -> (ringId - x) % ringId
            | x                     -> x % ringId

    location |> function
        | x when x <= 1 -> 0
        | x             -> 
            let (previousRing,limit) = rings |> Seq.takeWhile (snd >> ((>) x)) |> Seq.last
            match previousRing + 1 with
            | 1            -> 1 + (location % 2)
            | ringId       -> ringId + (rowOffset ringId limit)

let wrapGet (arr : int []) = function
    | x when x < 0 -> arr.[arr.Length + x]
    | x            -> arr.[x]

let generalIndexOffset rowLength location = 2 * (location / rowLength) + 1

let (|LastItem|_|) ((_, previousRing : int [], currentRing : int []), i) =
    if i = currentRing.Length - 1 
    then Some (currentRing.[i - 1] + currentRing.[0] + previousRing.[previousRing.Length - 1])
    else None

let (|BeforeLastItem|_|) ((_, previousRing : int [], currentRing : int []), i) =
    if i = currentRing.Length - 2 
    then Some (currentRing.[i - 1] + currentRing.[0] + previousRing.[previousRing.Length - 1] + previousRing.[previousRing.Length - 2])
    else None

let (|FirstItem|_|) ((_, previousRing : int [], _ : int []), i) =
    if i = 0 
    then Some (previousRing.[previousRing.Length - 1] + previousRing.[0])
    else None

let (|Corner|_|) ((rowLength, previousRing : int [], currentRing : int []), i) =
    if (i + 1) % rowLength = 0  
    then 
        let offset = 2 * ((i + 1) / rowLength)
        Some (currentRing.[i - 1] + previousRing.[i - offset])
    else None

let (|BeforeCorner|_|) ((rowLength, previousRing : int [], currentRing : int []), i) =
    if (i + 2) % rowLength = 0 
    then
        let offset = generalIndexOffset rowLength i // 2 * (i / rowLength) + 1
        Some (currentRing.[i - 1] + previousRing.[i - offset] + previousRing.[i - 1 - offset])
    else None

let (|AfterCorner|_|) ((rowLength, previousRing : int [], currentRing : int []), i) =
    if i % rowLength = 0
    then
        let offset = generalIndexOffset rowLength i  // 2  * (i / rowLength) + 1
        Some (currentRing.[i - 1] + currentRing.[i - 2] + previousRing.[i - offset]  + previousRing.[i + 1 - offset])
    else None

let calculateMiddle ((rowLength, previousRing : int [], currentRing : int []), i) =    
    let offset = generalIndexOffset rowLength i //2  * (i / rowLength) + 1
    let get = wrapGet previousRing
    currentRing.[i - 1] + get (i - 1 - offset) + get (i - offset) + get (i + 1 - offset)


let generateRing ringId previousRing =
    let rowLength = ringId * 2
    let ringSize = rowLength * 4
    let ring = Array.zeroCreate ringSize
    let data = (rowLength, previousRing, ring)

    for i in 0 .. (ringSize - 1) do
        let value = 
            match (data, i) with
            | FirstItem v      
            | LastItem v       
            | BeforeLastItem v 
            | BeforeCorner v   
            | Corner v         
            | AfterCorner v    -> v
            | x                -> calculateMiddle x
        ring.[i] <- value
    ring
    
let memoryValues = 
    seq {
        yield 1
        let rec loop i (ring : int []) =
            seq {
                yield! ring |> Seq.ofArray
                let nring = generateRing i ring
                yield! loop (i+1) nring
            }
        yield! loop 2 [|1;2;4;5;10;11;23;25|]
    }

let part2 inputValue = 
       memoryValues |> Seq.where (fun d -> d > inputValue) |> Seq.head