
let getTimers filename =
    System.IO.File.ReadAllText filename
    |> (fun str -> str.Split(','))
    |> Seq.map int
    |> Seq.toList

let decrement timer =
    if timer = 0 then [6;8] else [timer - 1]

let simulate days filename =
    let rec loop day timers =
        if day > days then
            timers
        else
            timers
            |> List.map decrement
            |> List.concat
            |> loop (day + 1)

    filename
    |> getTimers
    |> loop 1

let part1 filename days =
    filename
    |> simulate days
    |> List.length