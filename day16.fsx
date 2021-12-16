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

let rec parse (padded : bool) (input : seq<char>) =
    let advance (e : IEnumerator<char>) =
        e.MoveNext()

    let take cnt (e : IEnumerator<char>) =
        seq {
            for i in 1..cnt do
                let c = e.Current
                if i <> cnt then e.MoveNext() |> ignore
                yield c
        }

    let parseInt cnt e =
        e |> take cnt |> Seq.toArray |> String |> binToDec

    let parsePackageHeader (e : IEnumerator<char>) =
        let version = e |> parseInt 3
        e.MoveNext() |> ignore
        let type' = e |> parseInt 3
        { Version = version; Type = type' }

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

            if padded then
                let discard = 4 - (((groups * 5) + 6) % 4)
                for _ in 0..discard do
                    e.MoveNext() |> ignore
        |] |> String |> binToDec

    let parseOperator (e : IEnumerator<char>) =
        e.MoveNext() |> ignore
        let lenBits = if e.Current = '0' then 15 else 11
        e.MoveNext() |> ignore
        let len = e |> parseInt lenBits
        e.MoveNext() |> ignore

        e
        |> take len
        |> parse false
        |> Seq.toList

    seq {
        let e = input.GetEnumerator()

        while e.MoveNext() do
            let header = e |> parsePackageHeader
            let package =
                match header.Type with
                | 4 -> Literal(header, e |> parseLiteral)
                | _ -> Operator(header, e |> parseOperator)
            yield package
    }

let parseHex (input : string) =
    input
    |> hexToBin
    |> parse true

"00111000000000000110111101000101001010010001001000000000" |> parse true

"8A004A801A8002F478" |> parseHex

"110100101111111000101000" |> parse true

"00111000000000000110111101000101001010010001001000000000" |> parse true
"11010001010" |> parse false
"0101001000100100" |> parse false
"110100010100101001000100100" |> parse false


"110100101111111000101110100101111111000101" |> parse true