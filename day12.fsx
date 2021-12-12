type CaveSize =
    | Small
    | Big

type Cave = {
    Name: string
    Size: CaveSize
}

let createCave (str : string) =
    let size = if str.ToUpper() = str then Big else Small
    { Name = str; Size = size }

let start = createCave "start"
let end' = createCave "end"

let getConnections filename =
    filename
    |> System.IO.File.ReadAllLines
    |> Array.map (fun str ->
        match str.Split('-') with
        | [|l;r|] ->
            let left = createCave l
            let right = createCave r
            [|(left,right);(right,left)|]
        | _ -> [||])
    |> Array.concat
    |> Array.filter (fun (l,r) -> l <> end' && r <> start)
    |> Array.groupBy fst
    |> Array.map (fun (k,v) -> (k, v |> Array.map snd))
    |> Map.ofArray

let canVisit1 visited cave =
    match cave.Size with
    | Big -> true
    | Small -> not (visited |> List.contains cave)

let traverse canVisit (connections : Map<Cave, Cave[]>) =
    let rec loop (current : Cave) (visited : Cave list) : Cave list list =
        if current = end' then
            [end'::visited |> List.rev]
        else
            connections.[current]
            |> Array.Parallel.map (fun cave ->
                let visited = current::visited
                if cave |> canVisit visited then
                    visited |> loop cave
                else
                    [visited |> List.rev])
            |> List.concat

    loop start []
    |> List.distinct
    |> List.filter (fun l -> l |> List.contains end')

let part1 filename =
    filename
    |> getConnections
    |> traverse canVisit1
    |> List.length

// Part 2
let canVisit2 visited cave =
    let smallVisited =
        visited
        |> List.filter (fun c -> c.Size = Small)
        |> List.countBy (fun c -> c.Name)
        |> List.map snd
        |> List.append [0]
        |> List.max

    match cave.Size with
    | Big -> true
    | Small -> (not (visited |> List.contains cave)) || smallVisited < 2

let part2 filename =
    filename
    |> getConnections
    |> traverse canVisit2
    |> List.length