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

let (|Horizontal|_|) line =
    if line |> isHorizontal then
        let x1 = min line.A.X line.B.X
        let x2 = max line.A.X line.B.X
        Some [x1..x2]
    else None

let (|Vertical|_|) line =
    if line |> isVertical then
        let y1 = min line.A.Y line.B.Y
        let y2 = max line.A.Y line.B.Y
        Some [y1..y2]
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
    | Horizontal xs -> xs |> List.map (fun x -> { X = x; Y = line.A.Y })
    | Vertical ys -> ys |> List.map (fun y -> { X = line.A.X; Y = y })
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

"test.txt" |> getPoints |> List.fold addLine Map.empty |> toArray2D;;