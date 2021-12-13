type Axis =
    | XAxis
    | YAxis

type FoldInstruction = {
    Axis : Axis
    Index : int
}

let getInput filename =
    let lines =
        filename |> System.IO.File.ReadAllLines

    let dots =
        lines
        |> Seq.takeWhile (System.String.IsNullOrEmpty >> not)
        |> Seq.choose (fun str ->
            match str.Split(',') with
            | [|x;y|] -> Some (int x, int y)
            | _ -> None)
        |> Set.ofSeq

    let lenX = (dots |> Seq.maxBy (fun (x, _) -> x) |> fst) + 1
    let lenY = (dots |> Seq.maxBy (fun (_, y) -> y) |> snd) + 1

    let paper =
        Array2D.init lenY lenX (fun y x ->
            if dots |> Set.contains (x, y) then 1 else 0)

    let instructions =
        lines
        |> Seq.skipWhile (System.String.IsNullOrEmpty >> not)
        |> Seq.choose (fun line ->
            match line.Split(' ') with
            | [|"fold";"along";tokens|] ->
                match tokens.Split('=') with
                | [|axis;index|] ->
                    if axis = "x" then
                        Some { Axis = XAxis; Index = int index }
                    else if axis = "y" then
                        Some { Axis = YAxis; Index = int index }
                    else
                        None
                | _ -> None
            | _ -> None)
        |> Seq.toList

    (paper, instructions)

let split instruction (paper : int[,]) =
    if instruction.Axis = XAxis then
        let left = paper.[*, ..instruction.Index - 1]
        let right = paper.[*, instruction.Index + 1..]
        (left, right)
    else
        let top = paper.[..instruction.Index - 1, *]
        let bottom = paper.[instruction.Index + 1.., *]
        (top, bottom)

let flip axis (paper : int[,]) =
    if axis = XAxis then
        let max = (paper |> Array2D.length1) - 1
        [|0..max|]
        |> Array.map (fun y -> paper.[y, *] |> Array.rev)
        |> array2D
    else
        let max = (paper |> Array2D.length1) - 1
        [|0..max|]
        |> Array.map (fun y -> paper.[y, *])
        |> Array.rev
        |> array2D

let merge paper1 paper2 =
    let tryGet paper lenX lenY (x,y) =
        let offsetX = (paper |> Array2D.length2) - lenX
        let offsetY = (paper |> Array2D.length1) - lenY
        let maxX = (paper |> Array2D.length2) - 1
        let maxY = (paper |> Array2D.length1) - 1
        let (x, y) = (x + offsetX, y + offsetY)
        if x < 0 || x > maxX || y < 0 || y > maxY then 0 else paper.[y, x]

    let lenX = max (paper1 |> Array2D.length2) (paper2 |> Array2D.length2)
    let lenY = max (paper1 |> Array2D.length1) (paper2 |> Array2D.length1)

    Array2D.init lenY lenX (fun y x ->
        let p1 = tryGet paper1 lenX lenY (x, y)
        let p2 = tryGet paper2 lenX lenY (x, y)
        p1 + p2)

let applyInstruction paper instruction =
    paper
    |> split instruction
    |> (fun (p1, p2) -> (p1, p2 |> flip instruction.Axis))
    ||> merge

let part1 filename =
    let (paper, instructions) = getInput filename

    applyInstruction paper instructions[0]
    |> Seq.cast<int>
    |> Seq.filter (fun x -> x > 0)
    |> Seq.length


// let a = [|[|0;0;1;0;0;2;2|]|] |> array2D;;
// applyInstruction a { Axis = XAxis; Index =  4};;

// let b = [|[|0|];[|0|];[|1|];[|0|];[|0|];[|2|];[|2|]|] |> array2D;;
// applyInstruction b { Axis = YAxis; Index =  4};;