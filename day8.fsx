open System.Collections.Generic

type RegisterOperation = {StoreReg: string; Operation: string; Amount: int; CheckReg: string; CheckOp: string; CheckVal: int}
let parseLine (line : string) = 
    let ops = line.Split([|' '|])
    let (_, amount) = System.Int32.TryParse(ops.[2])
    let (_, checkVal) = System.Int32.TryParse(ops.[6])
    {StoreReg=ops.[0]
     Operation=ops.[1]
     Amount=amount
     CheckReg=ops.[4]
     CheckOp=ops.[5]
     CheckVal=checkVal}

let operations = System.IO.File.ReadAllLines(__SOURCE_DIRECTORY__ + "/inputs/day8.txt")
                 |> Array.map parseLine

let getRegisterValue (register : Dictionary<string,int>) (key : string) : int =
    if register.ContainsKey key then register.[key]
    else 0

let registerOperation register key x op = 
    let fn = if op = "dec" then (-) else (+)
    let value = getRegisterValue register key
    register.[key] <- fn value x

let booleanOp x y check =
    match check with 
    | "!=" -> x <> y
    | "<=" -> x <= y
    | "<"  -> x < y
    | ">=" -> x >= y
    | ">"  -> x > y
    | _    -> x = y


let processOp (register: Dictionary<string,int>) (regOp: RegisterOperation) = 
    let value = getRegisterValue register regOp.CheckReg
    if booleanOp value regOp.CheckVal regOp.CheckOp then 
        registerOperation register regOp.StoreReg regOp.Amount regOp.Operation 
    register 




operations 
  |> Seq.fold (fun acc x -> processOp acc x) (new Dictionary<string,int>())
  |> Seq.map (fun (kv) -> kv.Value)
  |> Seq.max


operations 
  |> Seq.scan (fun acc x -> processOp acc x) (new Dictionary<string,int>())
  |> Seq.map (fun regs -> if regs |> Seq.isEmpty then 0 else regs.Values |> Seq.maxBy id)
  |> Seq.max
