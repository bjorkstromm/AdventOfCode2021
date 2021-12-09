let getHeightMap filename =
    System.IO.File.ReadAllLines filename
    |> Array.map (fun str ->
        str.ToCharArray()
        |> Array.map (System.Char.GetNumericValue >> int))
    |> array2D

let isLowPoint map (x, y) =
    let maxY = (map |> Array2D.length1) - 1
    let maxX = (map |> Array2D.length2) - 1

    let minAdjacent =
        [(x-1,y);(x+1,y);(x,y-1);(x,y+1)]
        |> Seq.map (fun (x, y) ->
            if x >= 0 && x <= maxX && y >= 0 && y <= maxY then
                Some map.[y, x]
            else
                None)
        |> Seq.choose id
        |> Seq.min

    map.[y, x] < minAdjacent

let getLowPoints map =
    let maxY = (map |> Array2D.length1) - 1
    let maxX = (map |> Array2D.length2) - 1

    [0..maxY]
    |> Seq.map (fun y ->
        [0..maxX]
        |> Seq.map (fun x -> (x, y)))
    |> Seq.concat
    |> Seq.fold (fun acc (x, y) ->
        if isLowPoint map (x, y) then
            ((x, y), map.[y, x])::acc
        else
            acc) []

let getRiskLevel filename =
    filename
    |> getHeightMap
    |> getLowPoints
    |> List.map snd
    |> List.map ((+) 1)
    |> Seq.sum

// Part 2
let getBasin map (x, y) =
    let maxY = (map |> Array2D.length1) - 1
    let maxX = (map |> Array2D.length2) - 1

    let isInBounds (x, y) =
        x >= 0 && x <= maxX && y >= 0 && y <= maxY

    let isIncluded v (x, y) =
        isInBounds (x, y) &&
        map.[y, x] > v &&
        map.[y, x] < 9

    let rec loop state (x, y) =
        let current = map.[y, x]
        let included =
            [(x-1,y);(x+1,y);(x,y-1);(x,y+1)]
            |> List.except (state |> List.map fst)
            |> List.filter (isIncluded current)
            |> List.map (fun (x, y) -> ((x, y), map.[y, x]))

        if included.Length = 0 then
            state
        else
            let state = included @ state
            included
            |> List.map (fst >> (loop state))
            |> List.concat

    (x, y)
    |> loop [((x, y), map.[y, x])]
    |> List.distinct

let getBasins map lowPoints =
    lowPoints
    |> List.map (fst >> (getBasin map))

let part2 filename =
    let map = filename |> getHeightMap

    map
    |> getLowPoints
    |> getBasins map
    |> List.map List.length
    |> List.sortByDescending id
    |> List.take 3
    |> List.reduce (*)