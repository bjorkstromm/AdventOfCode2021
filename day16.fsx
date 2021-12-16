open System
open System.Collections.Generic

type PackageHeader = {
    Version : int
    Type : int
}

type Package =
    | Literal of (PackageHeader * int64)
    | Operator of (PackageHeader * Package list)

let hexToBin (str : seq<char>) =
    seq {
        for c in str do
            yield!
                (c.ToString(), 16)
                |> Convert.ToByte
                |> (fun b -> Convert.ToString(b, 2).PadLeft(4, '0'))
    }

let binToInt32 (str : string) =
    Convert.ToInt32(str, 2)

let binToInt64 (str : string) =
    Convert.ToInt64(str, 2)

let rec parse (input : seq<char>) =
    let advance (e : IEnumerator<char>) =
        e.MoveNext()

    let take cnt (e : IEnumerator<char>) =
        seq {
            let mutable continue' = true
            for i in 1..cnt do
                if continue' then
                    let c = e.Current
                    if i <> cnt then continue' <- e |> advance
                    yield c
        }

    let parseInt cnt e =
        let arr = e |> take cnt |> Seq.toArray
        if arr.Length <> cnt then None
        else arr |> String |> binToInt32 |> Some

    let parsePackageHeader (e : IEnumerator<char>) =
        if advance e then
            match e |> parseInt 3 with
            | None -> None
            | Some v ->
                if advance e then
                    match e |> parseInt 3 with
                    | None -> None
                    | Some t -> Some { Version = v; Type = t }
                else None
        else None

    let parseLiteral (e : IEnumerator<char>) =
        [|
            let mutable continue' = true
            while continue' do
                e.MoveNext() |> ignore
                continue' <- e.Current = '1'
                e.MoveNext() |> ignore
                let v = (e |> take 4)
                yield! v
        |] |> String |> binToInt64

    let rec parseOperator (e : IEnumerator<char>) =
        e.MoveNext() |> ignore
        let lenBits = if e.Current = '0' then 15 else 11
        e.MoveNext() |> ignore
        match e |> parseInt lenBits with
        | None -> []
        | Some len ->
            if lenBits = 15 then // length
                e.MoveNext() |> ignore
                e |> take len |> parse |> Seq.toList
            else // subpackets
                [0..len-1]
                |> List.choose (fun _ ->
                    match e |> parsePackageHeader with
                    | None -> None
                    | Some header ->
                        let package =
                            match header.Type with
                            | 4 -> Literal(header, e |> parseLiteral)
                            | _ -> Operator(header, e |> parseOperator)
                        Some package
                )

    let (|LiteralPackage|_|) (e, header) =
        if header.Type = 4 then
            Literal(header, e |> parseLiteral) |> Some
        else None

    let (|OperatorPackage|_|) (e, header) =
        if header.Type <> 4 then
            Operator(header, e |> parseOperator) |> Some
        else None

    seq {
        let e = input.GetEnumerator()

        let rec loop () = seq {
            match e |> parsePackageHeader with
            | Some header ->
                let package =
                    match (e, header) with
                    | LiteralPackage p -> p
                    | OperatorPackage p -> p
                    | _ -> failwithf "Invalid package header"

                yield package
                yield! loop ()
            | None -> () }

        yield! loop ()
    }

let parseHex (input : string) =
    input
    |> hexToBin
    |> parse

let rec sumVersion (packages : seq<Package>) =
    packages
    |> Seq.fold (fun sum p ->
        match p with
        | Literal (header, _) -> sum + header.Version
        | Operator (header, subs) ->
            subs |> sumVersion |> (+) (sum + header.Version)
        ) 0

let part1 filename =
    filename
    |> System.IO.File.ReadAllText
    |> parseHex
    |> sumVersion

// Part 2
let rec eval package =
    let reduce fn p =
        p |> List.map eval |> List.reduce (fn)

    let compare fn p =
        match p with
        | [rp;lp] ->
            let r = rp |> eval
            let l = lp |> eval
            if fn l r then 1L else 0L
        | p -> failwithf "Invalid number of packages %d" p.Length

    match package with
    | Literal (_, v) -> v
    | Operator (h, p) ->
        match (h.Type) with
        | 0 -> p |> reduce (+) // sum
        | 1 -> p |> reduce (*) // product
        | 2 -> p |> reduce (min) // minimum
        | 3 -> p |> reduce (max) // maximum
        | 5 -> p |> compare (>) // greater-than
        | 6 -> p |> compare (<) // less than
        | 7 -> p |> compare (=) // equal
        | t -> failwithf "Invalid operator type %d" t

let part2 filename =
    filename
    |> System.IO.File.ReadAllText
    |> parseHex
    |> Seq.map eval
    |> Seq.toList

"8A004A801A8002F478" |> parseHex |> sumVersion
"620080001611562C8802118E34" |> hexToBin |> parse |> sumVersion
"C0015000016115A2E0802F182340" |> parseHex |> sumVersion
"A0016C880162017C3686B18A3D4780" |> parseHex |> sumVersion

"C200B40A82" |> parseHex |> Seq.map eval
"04005AC33890" |> parseHex |> Seq.map eval
"880086C3E88112" |> parseHex |> Seq.map eval
"CE00C43D881120"  |> parseHex |> Seq.map eval
"D8005AC2A8F0" |> parseHex |> Seq.map eval
"F600BC2D8F" |> parseHex |> Seq.map eval
"9C005AC2F8F0" |> parseHex |> Seq.map eval
"9C0141080250320F1802104A08" |> parseHex |> Seq.map eval