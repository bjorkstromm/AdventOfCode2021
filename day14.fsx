let getData filename =
    let lines =
        System.IO.File.ReadAllLines filename

    let template =
        lines.[0]
        |> Seq.toList

    let insertionRules =
        lines.[2..]
        |> Seq.choose (fun str ->
            match str.Split(" -> ") with
            | [|l;r|] -> Some ((l.[0], l.[1]), r.[0])
            | _ -> None)
        |> Map.ofSeq

    (template, insertionRules)

let processData template (insertionRules : Map<(char * char), char>) =
    let template' =
        template
        |> List.pairwise
        |> List.map (fun (a, b) ->
            let c = insertionRules.[(a,b)]
            [a; c])
        |> List.concat

    template' @ [template.[^0]]

let part1 filename =
    let (template, insertionRules) = getData filename
    let folder template _ = processData template insertionRules

    let cnt =
         [1..10]
         |> List.fold folder template
         |> List.countBy id
         |> List.map snd

    let min = cnt |> List.min
    let max = cnt |> List.max
    max - min
