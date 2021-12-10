type ParseStatus =
    | Ok
    | Incomplete of char list
    | Corrupted of char

let openClosePairs =
    [('(', ')');('[', ']');('{', '}');('<', '>')]
    |> Map.ofList

let closings =
    openClosePairs
    |> Map.toSeq
    |> Seq.map snd
    |> Set.ofSeq

let (|Opening|_|) c =
    openClosePairs |> Map.tryFind c

let (|Closing|_|) c =
    if closings |> Set.contains c then Some c else None

let parseLine (line : string) =
    let rec loop stack chars =
        match chars with
        | [] -> if stack |> List.isEmpty then Ok else Incomplete stack
        | char::chars' ->
            match char with
            | Opening closing -> loop (closing::stack) chars'
            | Closing closing ->
                match stack with
                | [] -> Corrupted closing
                | expected::_ when expected <> closing -> Corrupted closing
                | _::stack' -> loop stack' chars'
            | _ -> Corrupted char

    line.ToCharArray()
    |> List.ofArray
    |> loop []

let points status =
    match status with
    | Ok -> 0
    | Incomplete _ -> 0
    | Corrupted char ->
        match char with
        | ')' -> 3
        | ']' -> 57
        | '}' -> 1197
        | '>' -> 25137
        | _ -> 0

let part1 filename =
    filename
    |> System.IO.File.ReadAllLines
    |> Array.Parallel.map parseLine
    |> Array.map points
    |> Array.sum