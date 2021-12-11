let getMap filename =
    filename
    |> System.IO.File.ReadAllLines
    |> Array.map (fun line ->
        line.ToCharArray()
        |> Array.map (System.Char.GetNumericValue >> int))
    |> array2D

// let getCoordinates map =
//     let yMax = (map |> Array2D.length1) - 1
//     let xMax = (map |> Array2D.length2) - 1
//     [0..yMax]
//     |> List.map (fun y ->
//         [0..xMax]
//         |> List.map (fun x -> (x, y)))
//     |> List.concat

let increaseByOne map =
    map |> Array2D.map ((+) 1)

let getAdjacent map (x,y) =
    let yMax = (map |> Array2D.length1) - 1
    let xMax = (map |> Array2D.length2) - 1
    [
        (x - 1, y - 1)
        (x, y - 1)
        (x + 1, y - 1)
        (x - 1, y)
        (x + 1, y)
        (x - 1, y + 1)
        (x, y + 1)
        (x + 1, y + 1)
    ] |> List.filter (fun (x, y) -> x >= 0 && x <= xMax && y >= 0 && y <= yMax)

let flash map =
    let rec loop map flashes =
        match flashes with
        | [] -> map
        | (x,y)::flashes ->
            let map' = map |> Array2D.copy
            let flashes' =
                (x,y)
                |> getAdjacent map
                |> List.map (fun (x, y) ->
                    map'.[y,x] <- map'.[y,x] + 1 // Mutating here, but maybe it's ok since it's copied
                    if map'.[y,x] = 10 then Some (x,y) else None)
                |> List.choose id
                |> List.append flashes

            loop map' flashes'

    map
    |> Array2D.mapi (fun y x v ->
        if v > 9 then Some (x, y) else None)
    |> Seq.cast<(int * int) option>
    |> Seq.choose id
    |> Seq.toList
    |> loop map

let zero map =
    map |> Array2D.map (fun v -> if v > 9 then 0 else v)

let executeStep map =
    map
    |> increaseByOne
    |> flash
    |> zero

let part1 filename iterations =
    let folder (n, map) _ =
        let map' = executeStep map
        let n' =
            map
            |> Seq.cast<int>
            |> Seq.filter (fun v -> v = 0)
            |> Seq.length
        (n + n', map')

    let map = getMap filename

    [0..iterations]
    |> List.fold folder (0, map)
    |> fst

// Part 2
let part2 filename =
    let rec loop map target i =
        let map' = executeStep map
        let sum =
            map
            |> Seq.cast<int>
            |> Seq.filter (fun v -> v = 0)
            |> Seq.length

        if sum = target then i else loop map' target (i + 1)

    let map = getMap filename
    let cnt = (map |> Array2D.length1) * (map |> Array2D.length2)

    loop map cnt 0
