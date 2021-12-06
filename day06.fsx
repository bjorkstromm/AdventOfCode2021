
let getTimers filename =
    System.IO.File.ReadAllText filename
    |> (fun str -> str.Split(','))
    |> Seq.map int
    |> Seq.toArray

let decrement timer =
    if timer = 0 then [|6;8|] else [|timer - 1|]

let simulate days filename =
    let rec loop day timers =
        if day > days then
            timers
        else
            timers
            |> Array.Parallel.map decrement
            |> Array.concat
            |> loop (day + 1)

    filename
    |> getTimers
    |> loop 1

let part1 filename days =
    filename
    |> simulate days
    |> Array.length

// Part 2
let getTimersMap filename =
    let map =
        System.IO.File.ReadAllText filename
        |> (fun str -> str.Split(','))
        |> Seq.map int
        |> Seq.countBy id
        |> Seq.map (fun (i, n) -> (i, int64 n))
        |> Map.ofSeq

    [0..8]
    |> Seq.map (fun i ->
        match Map.tryFind i map with
        | Some n -> (i, n)
        | None -> (i, 0))
    |> Map.ofSeq

let simulateMap days filename =
    let rec loop day (map : Map<int, int64>) =
        if day > days then
            map
        else
            [0..8]
            |> Seq.map (fun i ->
                if i = 6 then
                    (i, map.[0] + map.[7])
                else if i = 8 then
                    (i, map.[0])
                else
                    (i, map.[i + 1]))
            |> Map.ofSeq
            |> loop (day + 1)

    filename
    |> getTimersMap
    |> loop 1

let part2 filename days =
    filename
    |> simulateMap days
    |> Map.toSeq
    |> Seq.map snd
    |> Seq.sum