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
    |> Array.groupBy fst
    |> Array.map (fun (k,v) -> (k, v |> Array.map snd))
    |> Map.ofArray

let canVisit visited cave =
    match cave.Size with
    | Big -> true
    | Small -> not (visited |> List.contains cave)

let traverse (connections : Map<Cave, Cave[]>) =
    let start = createCave "start"
    let end' = createCave "end"
    
    let rec loop (current : Cave) (visited : Cave list) : Cave list list =
        if current = end' then
            [end'::visited |> List.rev]
        else
            connections.[current]
            |> Array.Parallel.map (fun cave ->
                if cave |> canVisit visited then
                    current::visited
                    |> loop cave
                else
                    [visited])
            |> List.concat

    loop start []
    |> List.distinct
    |> List.filter (fun l -> l |> List.contains end')

let part1 filename =
    filename
    |> getConnections
    |> traverse
    |> List.length