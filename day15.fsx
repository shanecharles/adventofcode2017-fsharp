let factorA = 16807L
let factorB = 48271L

let divisor = 2147483647L

let producer factor value =
    factor * value % divisor

let mask = System.Int64.Parse("FFFF", System.Globalization.NumberStyles.HexNumber)

let maskBits = ((&&&) mask)

let isPair (x : System.Int64, y : System.Int64) =
    maskBits x = maskBits y

let production factor = Seq.unfold (producer factor >> fun x -> Some(x, x))

let productionPairs startA startB = 
    Seq.zip (production factorA startA) (production factorB startB)

let producer' valid factor value =
    let rec loop v = v |> producer factor |> function
      | x when x |> valid -> x
      | x                 -> loop x
    loop value

let production' valid factor = Seq.unfold (producer' valid factor >> fun x -> Some(x,x))

let productionPairs' startA startB =
    Seq.zip 
      (production' (fun x -> x % 4L = 0L) factorA startA)
      (production' (fun x -> x % 8L = 0L) factorB startB)
//pairs = productionPairs factorA factorB 65L 8921L 
let part1 startA startB = 
    productionPairs startA startB 
      |> Seq.take 40000000 
      |> Seq.where isPair 
      |> Seq.length
      
let part2 startA startB = 
    productionPairs' startA startB 
      |> Seq.take 5000000 
      |> Seq.where isPair 
      |> Seq.length