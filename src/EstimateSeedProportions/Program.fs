open System

let gramsPerOz = 28.34952m
let seedsPerOz = [100000;78000;105000;27000;70000;12500;6600;3500]
let costPerOz = [8m;25m;40m;15m;40m;8m;8m;30m]
let costPerSeed = costPerOz |> List.zip seedsPerOz |> List.map (fun (s, c) -> c / decimal s)

// relative proportion to other seeds, seeds per gram
let seedsPerG = [(1.0m, 3527);(1.0m, 2751);(1.0m, 3704);(1.0m, 952);(1.0m, 2469);(1.0m, 441);(0.5m, 233);(0.5m, 123)]

// n is number of seeds per each flower type
let getProportions n = seedsPerG |> List.map (fun (rp, spg) -> rp * (decimal n) / (decimal spg))

let getOptimalSeedsPerEach targetG =
    Seq.unfold (fun n ->
                    let proportions = getProportions n
                    let total = proportions |> List.sum
                    if total < targetG then Some((n, total, proportions), n + 1) else None
                    ) 0
let (avgSeeds, totalG, proportions) = getOptimalSeedsPerEach 1.025m |> Seq.map (fun a -> printfn "%A" a; a) |> Seq.last
printfn ""
printfn "Std seeds/ea: %d" avgSeeds
printfn "Packet total g: %f" totalG
let calcs = proportions |> List.zip seedsPerG |> List.map (fun ((rp, spg), g) -> rp, (decimal spg * g), g)
calcs |> List.iter (fun (rp, s, g) -> printfn "Rel prop: %f, Seeds: %f, g: %f" rp s g)
let totals = 
    calcs
    |> List.reduce (fun (rp', s', g') (rp, s, g) -> 
                        (rp' + rp, s' + s, g' + g)
                    )
let (rp, s, g) = totals
printfn "TOTALS:    %f, Seeds: %f, g: %f" rp s g
Console.ReadLine() |> ignore