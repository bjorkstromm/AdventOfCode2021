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

// Part 2
let incompletePoints status =
    let folder total char =
        let total' = total * 5L
        match char with
        | ')' -> total' + 1L
        | ']' -> total' + 2L
        | '}' -> total' + 3L
        | '>' -> total' + 4L
        | _ -> total

    match status with
    | Ok -> None
    | Corrupted _ -> None
    | Incomplete stack ->
        stack
        |> List.fold folder 0L
        |> Some

let median sums =
    let index = (sums |> Array.length |> float) / 2. |> int
    let sorted = sums |> Array.sort
    sorted.[index]

let part2 filename =
    filename
    |> System.IO.File.ReadAllLines
    |> Array.Parallel.map parseLine
    |> Array.map incompletePoints
    |> Array.choose id
    |> median