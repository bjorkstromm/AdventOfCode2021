type Point = {
    X: int
    Y: int
}

type Line = {
    A: Point
    B: Point
}

let isHorizontal line = line.A.Y = line.B.Y

let isVertical line = line.A.X = line.B.X

let isDiagonal line =
    abs(line.A.X - line.B.X) = abs(line.A.Y - line.B.Y)

let (|Horizontal|_|) line =
    if line |> isHorizontal then
        let x1 = min line.A.X line.B.X
        let x2 = max line.A.X line.B.X
        [x1..x2]
        |> List.map (fun x -> { X = x; Y = line.A.Y })
        |> Some
    else None

let (|Vertical|_|) line =
    if line |> isVertical then
        let y1 = min line.A.Y line.B.Y
        let y2 = max line.A.Y line.B.Y
        [y1..y2]
        |> List.map (fun y -> { X = line.A.X; Y = y })
        |> Some
    else None

// Part 2
let (|Diagonal|_|) line =
    if line |> isDiagonal then
        let yDir = if line.A.Y < line.B.Y then 1 else -1
        let xDir = if line.A.X < line.B.X then 1 else -1

        let rec loop points point =
            match point with
            | p when p = line.B -> p::points |> List.rev
            | p ->
                let x = p.X + xDir
                let y = p.Y + yDir
                loop (p::points) { X = x; Y = y }

        loop [] line.A |> Some
    else None

let getPoints filename =
    let parsePoint (token : string) =
        match token.Split(",") with
        | [|x; y|] -> Some { X = x |> int; Y = y |> int }
        | _ -> None

    let parseLine (line : string) =
        match line.Split(" -> ") with
        | [|a; b|] ->
            match (parsePoint a, parsePoint b) with
            | (Some a, Some b) -> Some { A = a; B = b }
            | _ -> None
        | _ -> None

    System.IO.File.ReadAllLines filename
    |> Seq.map parseLine
    |> Seq.choose id
    |> Seq.toList

let getLinePoints line =
    match line with
    | Horizontal points -> points
    | Vertical points -> points
    | Diagonal points -> points
    | _ -> []

let addLine map line =
    let addPoint map point =
        map |>
        Map.change point (fun v ->
            match v with
            | Some v -> Some (v + 1)
            | None -> Some 1)

    line
    |> getLinePoints
    |> List.fold addPoint map

let getIntersections filename =
    filename
    |> getPoints
    |> List.fold addLine Map.empty
    |> Map.filter (fun _ v -> v > 1)
    |> Seq.length

let toArray2D map =
    let xMax = map |> Map.keys |> Seq.map (fun p -> p.X) |> Seq.max
    let yMax = map |> Map.keys |> Seq.map (fun p -> p.Y) |> Seq.max
    let arr = Array2D.zeroCreate (yMax + 1) (xMax + 1)

    map
    |> Map.toSeq
    |> Seq.iter (fun (k, v) ->
        arr.[k.Y, k.X] <- v)

    arr