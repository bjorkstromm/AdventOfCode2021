open System
open System.Collections.Generic

type PackageHeader = {
    Version : int
    Type : int
}

type Package =
    | Literal of (PackageHeader * int)
    | Operator of (PackageHeader * Package list)

let hexToBin (str : seq<char>) =
    seq {
        for c in str do
            yield!
                (c.ToString(), 16)
                |> Convert.ToByte
                |> (fun b -> Convert.ToString(b, 2).PadLeft(4, '0'))
    }

let binToDec (str : string) =
    Convert.ToInt32(str, 2)

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
        else arr |> String |> binToDec |> Some

    let parsePackageHeader (e : IEnumerator<char>) =
        if advance e then
            match e |> parseInt 3 with
            | None | Some 0 -> None
            | Some v ->
                if advance e then
                    match e |> parseInt 3 with
                    | None | Some 0 -> None
                    | Some t -> Some { Version = v; Type = t }
                else None
        else None

    let parseLiteral (e : IEnumerator<char>) =
        [|
            let mutable groups = 0
            let mutable continue' = true
            while continue' do
                groups <- groups + 1
                e.MoveNext() |> ignore
                continue' <- e.Current = '1'
                e.MoveNext() |> ignore
                let v = (e |> take 4)
                yield! v
        |] |> String |> binToDec

    let parseOperator (e : IEnumerator<char>) =
        e.MoveNext() |> ignore
        let lenBits = if e.Current = '0' then 15 else 11
        e.MoveNext() |> ignore
        let len = e |> parseInt lenBits |> Option.get
        e.MoveNext() |> ignore

        e
        |> take len
        |> parse
        |> Seq.toList

    seq {
        let e = input.GetEnumerator()

        let rec loop () = seq {
            match e |> parsePackageHeader with
            | Some header ->
                let package =
                    match header.Type with
                    | 4 -> Literal(header, e |> parseLiteral)
                    | _ -> Operator(header, e |> parseOperator)
                yield package
                yield! loop ()
            | None -> () }

        yield! loop ()
    }

let parseHex (input : string) =
    input
    |> hexToBin
    |> parse

"00111000000000000110111101000101001010010001001000000000" |> parse

"8A004A801A8002F478" |> parseHex

"110100101111111000101000" |> parse

"00111000000000000110111101000101001010010001001000000000" |> parse
"11010001010" |> parse
"0101001000100100" |> parse
"110100010100101001000100100" |> parse
"110100101111111000101110100101111111000101" |> parse

"110100101111111000101000" |> parse

"000" |> parse