let getData filename =
    let lines =
        System.IO.File.ReadAllLines filename

    let template =
        lines.[0]
        |> Seq.pairwise
        |> Seq.countBy id
        |> Seq.map (fun (k,v) -> (k, int64 v))
        |> Map.ofSeq

    let elementCount =
        lines.[0]
        |> Seq.countBy id
        |> Seq.map (fun (k,v) -> (k, int64 v))
        |> Map.ofSeq

    let insertionRules =
        lines.[2..]
        |> Seq.choose (fun str ->
            match str.Split(" -> ") with
            | [|l;r|] -> Some ((l.[0], l.[1]), r.[0])
            | _ -> None)
        |> Map.ofSeq

    (template, elementCount, insertionRules)

let processData 
    (pairs : Map<(char * char), int64>)
    (elementCount : Map<char, int64>)
    (insertionRules : Map<(char * char), char>) =

    let tmp = pairs |> Map.toSeq |> dict |> System.Collections.Generic.Dictionary
    let tmpCnt = elementCount |> Map.toSeq |> dict |> System.Collections.Generic.Dictionary

    pairs
    |> Map.toSeq
    |> Seq.iter (fun ((a,b), times) ->
            let c = insertionRules.[(a,b)]

            let ac = match tmp.TryGetValue((a,c)) with | (true, v) -> v | _ -> 0
            tmp.[(a,c)] <- (ac + times)

            let cb = match tmp.TryGetValue((c,b)) with | (true, v) -> v | _ -> 0
            tmp.[(c,b)] <- (cb + times)

            let ab = match tmp.TryGetValue((a,b)) with | (true, v) -> v | _ -> 0
            tmp.[(a,b)] <- (ab - times)

            let cv = match tmpCnt.TryGetValue(c) with | (true, v) -> v | _ -> 0
            tmpCnt.[c] <- cv + times
        )

    let pairs =
        tmp
        |> Seq.map (|KeyValue|)
        |> Seq.filter (fun (_,v) -> v > 0L)
        |> Map.ofSeq

    let elementCount =
        tmpCnt
        |> Seq.map (|KeyValue|)
        |> Map.ofSeq

    (pairs, elementCount)

let execute filename times =
    let (template, elementCount, insertionRules) = getData filename
    let folder (template, elementCount) _ =
        processData template elementCount insertionRules

    let cnt =
        [1..times]
        |> List.fold folder (template, elementCount)
        |> (fun (_, cnt) ->
            cnt
            |> Map.toSeq
            |> Seq.map snd)

    let min = cnt |> Seq.min
    let max = cnt |> Seq.max
    max - min

let part1 filename =
    execute filename 10

let part2 filename =
    execute filename 40