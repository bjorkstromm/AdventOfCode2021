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

// Part 2
type Row = {
    SignalPattern : char list list
    Output : char list list
}

let getData filename =
    let parseToken (token : string) =
        token.Split(' ')
        |> Seq.map (fun str -> str.ToCharArray() |> Array.sort |> Array.toList)
        |> Seq.toList

    let parseRow (row : string) =
        match row.Split(" | ") with
        | [|signals; output|] -> Some {
            SignalPattern = signals |> parseToken
            Output = output |> parseToken }
        | _ -> None

    System.IO.File.ReadAllLines filename
    |> Seq.map parseRow
    |> Seq.choose id
    |> Seq.toList

let (|One|_|) pattern =
    if isOne pattern then Some (1, pattern) else None

let (|Four|_|) pattern =
    if isFour pattern then Some (4, pattern) else None

let (|Seven|_|) pattern =
    if isSeven pattern then Some (7, pattern) else None

let (|Eight|_|) pattern =
    if isEight pattern then Some (8, pattern) else None

let mapSegments (patterns : char list list) =
    let includes segments pattern =
        segments
        |> List.forall (fun segment ->
            pattern |> List.contains segment)

    let findPattern num len (inc : int list) (inc2 : int list) (map : Map<int, char list>) (patterns : char list list) =
        let contains = inc |> Seq.map (fun num -> map.[num])
        let contained = inc2 |> Seq.map (fun num -> map.[num])
        let pattern = 
            patterns
            |> Seq.filter (fun p ->
                p.Length = len &&
                contains |> Seq.forall (fun s -> p |> includes s) &&
                contained |> Seq.forall (fun s -> s |> includes p))
            |> Seq.head

        (map |> Map.add num pattern, patterns |> List.except [pattern])

    let (map, patterns) =
        patterns
        |> Seq.map (fun pattern ->
            match pattern with
            | One v -> Some v
            | Four v -> Some v
            | Seven v -> Some v
            | Eight v -> Some v
            | _ -> None)
        |> Seq.choose id
        |> Map.ofSeq
        |> (fun m -> (m, patterns |> List.except m.Values))

    (map, patterns)
    ||> findPattern 3 5 [7] [] // 3 is five segments, and contains 7
    ||> findPattern 9 6 [3] [] // 9 is six segments, and contains 3
    ||> findPattern 0 6 [7] [] // 0 is six segments, and contains 7
    ||> findPattern 6 6 [] [] // 6 is six segments
    ||> findPattern 5 5 [] [6] // 5 is five segments, and is contained in 6
    ||> findPattern 2 5 [] [] // 2 is five segments
    |> fst
    |> Map.toSeq
    |> Seq.map (fun (k, v) -> (v, k))
    |> Map.ofSeq


let part2 filename =
    let rows = filename |> getData
    rows
    |> Seq.map (fun row -> 
        let map = row.SignalPattern |> mapSegments
        row.Output
        |> Seq.rev
        |> Seq.mapi (fun i pattern -> map.[pattern] * (int)(10. ** i))
        |> Seq.sum)
    |> Seq.sum