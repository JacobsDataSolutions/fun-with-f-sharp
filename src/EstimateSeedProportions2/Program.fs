open System
type Seed = { Name: string; LatinName: string; SeedsPerG: int; CostPerG: decimal; RelativeProportionInPack: decimal; SupplyOz: decimal }
type SeedComponentInPack = { Name: string; RelativeProportionInPack: decimal; NumSeeds: int; GramDist: decimal; Cost: decimal }
let gramsPerOz = 28.34952m
// Proportions are base-case, align w/ spreadsheet
let seedsInitial = [
        { Name = "Black-eyed Susan"; LatinName = "Rudbeckia hirta"; SeedsPerG = 3527; CostPerG = 0.282191727m; RelativeProportionInPack = 1.0m; SupplyOz = 1.0m };
        { Name = "Wild Bergamot"; LatinName = "Monarda fistulosa"; SeedsPerG = 2751; CostPerG = 0.881849146m; RelativeProportionInPack = 1.0m; SupplyOz = 1.0m };
        { Name = "Showy Goldenrod"; LatinName = "Solidago speciosa"; SeedsPerG = 3704; CostPerG = 1.410958634m; RelativeProportionInPack = 1.0m; SupplyOz = 1.0m };
        { Name = "Yellow Prairie Coneflower"; LatinName = "Ratibida pinnata"; SeedsPerG = 952; CostPerG = 0.529109488m; RelativeProportionInPack = 1.0m; SupplyOz = 1.0m };
        { Name = "New England Aster"; LatinName = "Symphyotrichum novae-angliae"; SeedsPerG = 2469; CostPerG = 1.410958634m; RelativeProportionInPack = 1.0m; SupplyOz = 1.0m };
        { Name = "Lance Leaved Coreopsis"; LatinName = "Coreopsis lanceolata"; SeedsPerG = 441; CostPerG = 0.282191727m; RelativeProportionInPack = 1.0m; SupplyOz = 1.0m };
        { Name = "Purple Coneflower"; LatinName = "Echinacea purpurea"; SeedsPerG = 233; CostPerG = 0.282191727m; RelativeProportionInPack = 1.0m; SupplyOz = 1.0m };
        { Name = "Butterfly Milkweed"; LatinName = "Asclepias tuberosa"; SeedsPerG = 123; CostPerG = 1.058218975m; RelativeProportionInPack = 1.0m; SupplyOz = 1.0m }
    ]

// n is number of seeds per each flower type
let getProportions (seeds: Seed list) (n: int) = seeds |> List.map (fun seed -> seed.RelativeProportionInPack * (decimal n) / (decimal seed.SeedsPerG))

// targetG is the target weight in grams for the pack (not to be exceeded)
let getOptimalSeedsPerEach (seeds: Seed list) (targetG: decimal) =
    Seq.unfold (fun n ->
                    let proportions = getProportions seeds n
                    let total = proportions |> List.sum
                    if total < targetG then Some(proportions |> List.zip seeds |> List.map (fun (s, p) -> { Name = s.Name; RelativeProportionInPack = s.RelativeProportionInPack; NumSeeds = int (Math.Ceiling(decimal s.SeedsPerG * p)); GramDist = p; Cost = p * s.CostPerG }), n + 1) else None
                    ) 0

let optimalPackSeedList = getOptimalSeedsPerEach seedsInitial 1.025m |> Seq.map (fun a -> printfn "%A" a; a) |> Seq.last
let avgSeeds = optimalPackSeedList |> List.map (fun c -> decimal c.NumSeeds) |> List.average
let totalG = optimalPackSeedList |> List.map (fun c -> c.GramDist) |> List.sum
printfn ""
printfn "Std seeds/ea: %.2f" avgSeeds
printfn "Packet total g: %f" totalG

optimalPackSeedList |> List.iter (fun c -> printfn "%30s Rel prop: %f, Seeds: %d, g: %f, c: %.2f" c.Name c.RelativeProportionInPack c.NumSeeds c.GramDist c.Cost)
let totals = optimalPackSeedList |> List.reduce (fun s' s -> { Name = "TOTALS"; RelativeProportionInPack = s'.RelativeProportionInPack + s.RelativeProportionInPack; NumSeeds = s'.NumSeeds + s.NumSeeds; GramDist = s'.GramDist + s.GramDist; Cost = s'.Cost + s.Cost })

printfn "%30s           %f, Seeds: %d, g: %f, c: %.2f" totals.Name totals.RelativeProportionInPack totals.NumSeeds totals.GramDist totals.Cost
Console.ReadLine() |> ignore