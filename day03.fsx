let getDiagnosticReport filename =
    System.IO.File.ReadAllLines filename
    |> Array.map (fun line ->
        line.ToCharArray()
        |> Array.map (System.Char.GetNumericValue >> int))
    |> array2D

let getMSB (report : int [,]) =
    let len = report |> Array2D.length2
    let middle = (report |> Array2D.length1) / 2

    let sum i = report.[*,i] |> Array.sum
    let toBit n = if n > middle then 1 else 0

    [|0..(len - 1)|]
    |> Array.map (sum >> toBit)

let invert bits =
    bits
    |> Array.map (fun bit -> if bit = 0 then 1 else 0)

let toDecimal bits =
    let convert (i : int) (n : int) =
        (float)n * (2. ** i) |> int

    bits
    |> Array.rev
    |> Array.mapi convert
    |> Array.sum

// Part 1
let getPowerConsumption filename =
    let msb = getDiagnosticReport filename |> getMSB
    let lsb = msb |> invert
    let gamma = msb |> toDecimal
    let epsilon = lsb |> toDecimal

    (gamma * epsilon)