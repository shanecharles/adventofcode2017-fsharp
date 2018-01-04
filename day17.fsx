let insertAfter (l : int list) i v = 
    l.[0..i] @ (v :: l.[i+1..])

let spinLock spin (l : int list) i =
    let len = l |> List.length
    (spin + i) % len

let iterate spins insert index l =
    let nIndex = spinLock spins l index
    let nData = insertAfter l nIndex insert
    nIndex + 1, nData

let part1 spins = 
    (1,0,[0])
      |> Seq.unfold (fun (c, i, l) -> 
            let ni, nl = iterate spins c i l
            Some ((ni,nl), (c+1, ni, nl))) 
      |> Seq.skip 2016
      |> Seq.head
      |> snd
      |> Seq.skipWhile ((<>)2017)
      |> Seq.skip 1
      |> Seq.head

let spinLock' spin len i =
    (spin + i) % len

let part2 spins = 
    (1,0) 
      |> Seq.unfold (fun (l, i) -> 
            let ni = i |> spinLock' spins l |> ((+)1)
            Some ((l, ni), (l+1, ni)))
      |> Seq.take 50000000 
      |> Seq.where (snd >> ((=)1))
      |> Seq.last
      |> fst