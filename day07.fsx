let getPositions filename =
    System.IO.File.ReadAllText filename
    |> (fun str -> str.Split(','))
    |> Seq.map int
    |> Seq.countBy id
    |> Map.ofSeq

let getAllPositions (positions : Map<int, int>) =
    let min = positions.Keys |> Seq.min
    let max = positions.Keys |> Seq.max
    [min..max]

let calculateFuelForPosition (positions : Map<int, int>) i =
    positions
    |> Map.toSeq
    |> Seq.map (fun (k, v) -> abs(k - i) * v)
    |> Seq.sum

let getCheapestPosition filename =
    let positions = getPositions filename
    positions
    |> getAllPositions
    |> Seq.map (fun i -> (i, calculateFuelForPosition positions i))
    |> Seq.minBy (fun (_, fuel) -> fuel)