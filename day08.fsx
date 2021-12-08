let getOutputValues filename =
    System.IO.File.ReadAllLines filename
    |> Seq.map (fun str ->
        str.Split(" | ")
        |> Array.last
        |> (fun str ->
            str.Split(' ')
            |> Array.map (fun str -> str.ToCharArray() |> Array.toList)
            |> Array.toList))
    |> Seq.toList


let isOne segments = Seq.length segments = 2
let isFour segments = Seq.length segments = 4
let isSeven segments = Seq.length segments = 3
let isEight segments = Seq.length segments = 7

let part1 filename =
    filename
    |> getOutputValues
    |> Seq.fold (fun acc outputs ->
        let cnt =
            outputs
            |> Seq.filter (fun segments ->
                match segments with
                | _ when isOne segments -> true
                | _ when isFour segments -> true
                | _ when isSeven segments -> true
                | _ when isEight segments -> true
                | _ -> false)
            |> Seq.length
        cnt + acc) 0