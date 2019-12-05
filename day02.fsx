open System

let restored =
    let raw = "1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,6,19,1,19,5,23,2,13,23,27,1,10,27,31,2,6,31,35,1,9,35,39,2,10,39,43,1,43,9,47,1,47,9,51,2,10,51,55,1,55,9,59,1,59,5,63,1,63,6,67,2,6,67,71,2,10,71,75,1,75,5,79,1,9,79,83,2,83,10,87,1,87,6,91,1,13,91,95,2,10,95,99,1,99,6,103,2,13,103,107,1,107,2,111,1,111,9,0,99,2,14,0,0"
    let crashed = raw.Split(',') |> Seq.mapi(fun i n -> (i,int n)) |> Map.ofSeq
    crashed.Add(1,12).Add(2,2)

let rec run p (mem:Map<int,int>) =    
    let (op,v1,v2,pRet) = mem.[p], mem.[mem.[p+1]], mem.[mem.[p+2]], mem.[p+3]
    match op with
    | 1 -> run (p+4) (mem.Add(pRet, (v1 + v2)))
    | 2 -> run (p+4) (mem.Add(pRet, (v1 * v2)))
    | 99 -> mem
    | n -> failwith (sprintf "invalid operation %i at position %i" n p)

printfn "Day 2 part 1: %i" (run 0 restored).[0]

let solver (prog:Map<int,int>) target =
    let run noun verb = (run 0 (prog.Add(1,noun).Add(2,verb))).[0]
    let min,max = 0,99
    let rec solve noun verb =
        match run noun verb with
        | n when n = target -> (noun, verb)
        | _ when verb = max && noun = max -> (-1, -1)
        | _ when verb = max -> solve (noun+1) min
        | _ -> solve noun (verb+1)
    solve min max

do 
    let ansNoun, ansVerb = solver restored 19690720
    printfn "Day 2 part 2: %i" (100 * ansNoun + ansVerb)

