let getBingoInputs (str : string) =
    str.Split(',')
    |> Array.map int
    |> Array.toList

let getBingoBoard (lines : seq<string>) =
    let toNum (str : string) =
        ((str |> int), false)
    lines
    |> Seq.map (fun line ->
        line.Split(' ', System.StringSplitOptions.RemoveEmptyEntries)
        |> Array.map toNum)
    |> array2D

let getData filename =
    let lines = Array.append (System.IO.File.ReadAllLines filename) [|""|]
    let inputs = lines.[0] |> getBingoInputs

    let boards =
        lines.[2..]
        |> Array.fold (fun (boards, lines) line ->
                if line = "" then
                    let board = lines |> List.rev |> getBingoBoard
                    (board::boards, [])
                else
                    (boards, line::lines)
            ) ([], [])
        |> fst
        |> List.rev
        |> List.toArray

    (inputs, boards)

let getCoordinatesOf n board =
    let xMax = (board |> Array2D.length2) - 1
    let yMax = (board |> Array2D.length1) - 1 
    let rec loop x y =
        let (v, _) = board.[y,x]
        if v = n then
            Some (y,x)
        else
            let x = if x = xMax then 0 else x + 1
            let y = if x = 0 then y + 1 else y
            if y > yMax then None else loop x y

    loop 0 0

let isBingo (y,x) (board : (int * bool)[,]) =
    let check (_, m) = m = true
    let v = board.[y,*] |> Array.forall check
    let h = board.[*,x] |> Array.forall check
    v || h

let addNumToBoard n (board : (int * bool)[,]) =
    match getCoordinatesOf n board with
    | None -> (false, board)
    | Some (y,x) ->
        let board = board |> Array2D.copy
        board.[y,x] <- (n, true)
        let isBingo = isBingo (y,x) board
        (isBingo, board)

// Part 1
let playBingo filename =
    let rec loop (numbers : int list) (boards : (int * bool)[,][]) =
        match numbers with
        | [] -> None
        | head::tail ->
            let newBoards =
                boards
                |> Array.Parallel.map (fun b -> b |> addNumToBoard head)

            let winners =
                newBoards
                |> Array.filter (fun (w, _) -> w)
                |> Array.map snd

            if winners.Length > 0 then
                Some (head, winners[0])
            else
                loop tail (newBoards |> Array.map snd)

    let (numbers, boards) = getData filename

    loop numbers boards

let sumUnmarked (board : (int * bool)[,]) =
    let mutable sum = 0

    board
    |> Array2D.iter (fun (v, m) ->
        if m = false then sum <- sum + v else ())

    sum

let part1 filename =
    match playBingo filename with
    | None -> 0
    | Some (n, board) ->
        let sum = sumUnmarked board
        sum * n