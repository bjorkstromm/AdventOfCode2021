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

// Part 2
let getByBitCriteria (includeZerosFunc : int -> int -> bool) (report : int[,]) =
    let rec loop i (report : int[,]) =
        let len = report |> Array2D.length1

        if len = 1 then
            report.[0,*]
        else
            let arr = report.[*,i]
            let ones = arr |> Array.filter (fun n -> n = 1) |> Array.length
            let zeros = len - ones
            let includeZeros = includeZerosFunc zeros ones
            let includeOnes = not(includeZeros)

            [|
                for j in 0..(len - 1) do
                    if (report.[j,i] = 0 && includeZeros) || (report.[j,i] = 1 && includeOnes) then
                        yield report.[j,*]
            |] |> array2D |> loop (i + 1)

    loop 0 report

let getOxygenGeneratorRating (report : int[,]) =
    report |> getByBitCriteria (fun zeros ones -> zeros > ones)

let getCO2ScrubberRating (report : int[,]) =
    report |> getByBitCriteria (fun zeros ones -> not(zeros > ones))

let getLifeSupportRating filename =
    let report = getDiagnosticReport filename
    let oxygen = getOxygenGeneratorRating report |> toDecimal
    let scrubber = getCO2ScrubberRating report |> toDecimal
    (oxygen * scrubber)