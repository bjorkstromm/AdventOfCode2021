
type Direction =
    | Increased
    | Decreased
    | Unchanged

let getMeasurements filename =
    System.IO.File.ReadAllLines filename
    |> Array.map int
    |> Array.toList

let processMeasurements measurements =
    let getDirection (a, b) =
        if a > b then Decreased
        else if a < b then Increased
        else Unchanged

    let tail =
        measurements
        |> Seq.pairwise
        |> Seq.map (fun (a, b) -> (b, getDirection (a, b)))
        |> Seq.toList

    let head = (measurements |> Seq.head, Unchanged)

    head::tail

// Part 1
let getIncreases filename =
    filename
    |> getMeasurements
    |> processMeasurements
    |> Seq.filter (fun (a, b) -> b = Increased)
    |> Seq.length

// Part 2
let processWindowed (measurements : seq<int>) =
    measurements
    |> Seq.windowed 3
    |> Seq.map (fun m -> Array.sum m)
    |> processMeasurements

let getIncreasesWindowed filename =
    filename
    |> getMeasurements
    |> processWindowed
    |> Seq.filter (fun (a, b) -> b = Increased)
    |> Seq.length