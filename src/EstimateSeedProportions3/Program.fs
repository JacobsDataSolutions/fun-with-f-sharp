open System
open System.IO
open DecimalMath

let variance (values: decimal seq) = 
    let average = Seq.average values
    let length = Seq.length values
    let sum = values |> Seq.map (fun x -> (x - average) * (x - average)) |> Seq.sum
    sum / decimal length

let stdDeviation (values: decimal seq) = DecimalEx.Sqrt(variance values)    

let meanDeviation (values: decimal seq) =
    let average = Seq.average values
    let length = Seq.length values
    let sum = values |> Seq.map (fun x -> Math.Abs(x - average)) |> Seq.sum
    sum / decimal length

type Seed = { Name: string; LatinName: string; SeedsPerG: decimal; SupplyCost: decimal; RelativeProportionInPack: decimal; SupplyOz: decimal; SupplyG: decimal }
type SeedComponentInPack = { Name: string; RelativeProportionInPack: decimal; NumSeeds: decimal; Grams: decimal; Cost: decimal; SeedPercentOfPack: decimal; SupplyOz: decimal }
type Cost = { SupplyOz: decimal; Cost: decimal }
type SeedSupply = { Name: string; LatinName: string; SeedsPerG: decimal; Costs: Cost list }
type Pack = { ComboId: int; SeedComponentsInPack: SeedComponentInPack list; TotalCost: decimal; GrossProfit: decimal; NetProfit: decimal; NetProfitPercent: decimal; NumPackets: decimal; SupplyTotalG: decimal; AvgSeeds: decimal; SeedsVariance: decimal; SeedsMeanDev: decimal; SeedsStdDev: decimal; SeedsPerPack: decimal }
let gramsPerOz = 28.34952m

let prarieNurseryCosts = [
    { Name = "Black-eyed Susan"; LatinName = "Rudbeckia hirta"; SeedsPerG = 3527m; Costs = [ { SupplyOz = 0.25m; Cost = 3m }; { SupplyOz = 0.5m; Cost = 5m }; { SupplyOz = 1m; Cost = 8m }; { SupplyOz = 16m; Cost = 120m } ] };
    { Name = "Wild Bergamot"; LatinName = "Monarda fistulosa"; SeedsPerG = 2751m; Costs = [ { SupplyOz = 0.25m; Cost = 10m }; { SupplyOz = 0.5m; Cost = 15m }; { SupplyOz = 1m; Cost = 25m }; { SupplyOz = 16m; Cost = 375m } ] };
    { Name = "Showy Goldenrod"; LatinName = "Solidago speciosa"; SeedsPerG = 3704m; Costs = [ { SupplyOz = 0.25m; Cost = 16m }; { SupplyOz = 0.5m; Cost = 24m }; { SupplyOz = 1m; Cost = 40m }; { SupplyOz = 16m; Cost = 600m } ] };
    { Name = "Yellow Prairie Coneflower"; LatinName = "Ratibida pinnata"; SeedsPerG = 952m; Costs = [ { SupplyOz = 0.25m; Cost = 6m }; { SupplyOz = 0.5m; Cost = 9m }; { SupplyOz = 1m; Cost = 15m }; { SupplyOz = 16m; Cost = 225m } ] };
    { Name = "New England Aster"; LatinName = "Symphyotrichum novae-angliae"; SeedsPerG = 2469m; Costs = [ { SupplyOz = 0.25m; Cost = 16m }; { SupplyOz = 0.5m; Cost = 24m }; { SupplyOz = 1m; Cost = 40m }; { SupplyOz = 16m; Cost = 600m } ] };
    { Name = "Lance Leaved Coreopsis"; LatinName = "Coreopsis lanceolata"; SeedsPerG = 441m; Costs = [ { SupplyOz = 0.25m; Cost = 3m }; { SupplyOz = 0.5m; Cost = 5m }; { SupplyOz = 1m; Cost = 8m }; { SupplyOz = 16m; Cost = 120m } ] };
    { Name = "Purple Coneflower"; LatinName = "Echinacea purpurea"; SeedsPerG = 233m; Costs = [ { SupplyOz = 0.25m; Cost = 3m }; { SupplyOz = 0.5m; Cost = 5m }; { SupplyOz = 1m; Cost = 8m }; { SupplyOz = 16m; Cost = 120m } ] };
    { Name = "Butterfly Milkweed"; LatinName = "Asclepias tuberosa"; SeedsPerG = 123m; Costs = [ { SupplyOz = 0.25m; Cost = 12m }; { SupplyOz = 0.5m; Cost = 8m }; { SupplyOz = 1m; Cost = 30m }; { SupplyOz = 16m; Cost = 450m } ] }
]

let calculateCosts packId seedsInitial =
    let seedsInitialWGrams = seedsInitial |> List.map (fun s -> { s with SupplyG = s.SupplyOz * gramsPerOz })

    let packSize = 1.0m
    let additionalCosts = 55.0m
    let salePricePerPack = 4.95m
    let totalG = seedsInitialWGrams |> List.sumBy (fun s -> s.SupplyG)
    let numPacks = Math.Floor(totalG / packSize)
    let packList = seedsInitialWGrams |> List.map (fun s -> 
                                                    let gramsPerPack = s.SupplyG / numPacks
                                                    let costPerGram = s.SupplyCost / s.SupplyOz / gramsPerOz
                                                    let costPerPack = costPerGram * gramsPerPack
                                                    let numSeeds = s.SeedsPerG * gramsPerPack
                                                    let percentOfPack = 0.0m
                                                    { Name = s.Name; RelativeProportionInPack = s.RelativeProportionInPack; Grams = gramsPerPack; Cost = costPerPack; NumSeeds = numSeeds; SeedPercentOfPack = percentOfPack; SupplyOz = s.SupplyOz })

    let totals = packList |> List.reduce (fun s' s -> { Name = "TOTALS"; SupplyOz = s'.SupplyOz + s.SupplyOz; RelativeProportionInPack = s'.RelativeProportionInPack + s.RelativeProportionInPack; NumSeeds = s'.NumSeeds + s.NumSeeds; Grams = s'.Grams + s.Grams; Cost = s'.Cost + s.Cost; SeedPercentOfPack = s'.SeedPercentOfPack + s.SeedPercentOfPack })
    let packList2 = packList |> List.map (fun p -> { p with SeedPercentOfPack = p.NumSeeds / totals.NumSeeds * 100.0m })
    let totalCost = totals.Cost * numPacks + additionalCosts
    let grossProfit = salePricePerPack * numPacks
    let potentialProfit = grossProfit - totalCost
    let nums = packList2 |> List.map (fun p -> p.NumSeeds)
    let avgSeeds = nums |> Seq.average
    let variance = nums |> variance
    let meanDev = nums |> meanDeviation
    let stdDev = nums |> stdDeviation
    let netProfitPercent = potentialProfit / totalCost * 100.0m;
    { ComboId = packId; SeedComponentsInPack = packList2; TotalCost = totalCost; GrossProfit = grossProfit; NetProfit = potentialProfit; NetProfitPercent = netProfitPercent; NumPackets = numPacks; SupplyTotalG = totalG; AvgSeeds = avgSeeds; SeedsVariance = variance; SeedsPerPack = totals.NumSeeds; SeedsMeanDev = meanDev; SeedsStdDev = stdDev }

let seedNames = prarieNurseryCosts |> List.map (fun s -> s.Name)
let allSeedsWCosts = [
    for plant in prarieNurseryCosts do
        for cost in plant.Costs do
            yield { Name = plant.Name; LatinName = plant.LatinName; SeedsPerG = plant.SeedsPerG; RelativeProportionInPack = 1.0m; SupplyOz = cost.SupplyOz; SupplyCost = cost.Cost; SupplyG = cost.SupplyOz * gramsPerOz }
]
let groupedSeeds = allSeedsWCosts |> Array.ofList |> Array.groupBy (fun s -> s.Name)
let variationMatrix = Array2D.init 8 4 (fun i j -> (snd groupedSeeds[i])[j])
let allCombos = [
    for a in [0..3] do
        for b in [0..3] do
            for c in [0..3] do
                for d in [0..3] do
                    for e in [0..3] do
                        for f in [0..3] do
                            for g in [0..3] do
                                for h in [0..3] do
                                    yield [ variationMatrix[0, a]; variationMatrix[1, b]; variationMatrix[2, c]; variationMatrix[3, d]; variationMatrix[4, e]; variationMatrix[5, f]; variationMatrix[6, g]; variationMatrix[7, h] ]
]

let outFile = File.CreateText("outfile.csv")

let sortedPacksCombos = allCombos |> List.mapi calculateCosts |> List.sortBy (fun p -> p.TotalCost, p.SeedsStdDev)
//let selectedPacks = sortedPacksCombos |> List.filter (fun p -> p.TotalCost < 200.0m && p.NetProfitPercent > 300.0m && p.SeedsVariance < 5000.0m)
//let selectedPacks = sortedPacksCombos |> List.filter (fun p -> p.NetProfitPercent > 300.0m && p.SeedsVariance < 1000.0m)
//let selectedPacks = sortedPacksCombos //|> List.filter (fun p -> p.NetProfitPercent > 300.0m && p.SeedsVariance < 1000.0m)

// Within x std devs
let inRange avg stdDev numStdDevs n =
    let top = avg + (numStdDevs * stdDev)
    let bottom = avg - (numStdDevs * stdDev)
    n <= top && n >= bottom

let selectedPacks = sortedPacksCombos |> List.filter (fun p -> (p.SeedComponentsInPack |> List.forall (fun s -> inRange p.AvgSeeds p.SeedsStdDev 1.255m s.NumSeeds)))
let selectedPacks2 = selectedPacks |> List.filter (fun p -> p.TotalCost <= 300.0m) 
//let selectedPacks2 = selectedPacks |> List.filter (fun p -> p.SeedComponentsInPack[7].SeedPercentOfPack >= 3.0m)

fprintfn outFile "Combo #,Mean Dev,Std Dev,Avg Seeds,Seeds/Pack,Total Cost,Gross Profit,Net Profit,Profit %%,Num Packs,A/oz,B/oz,C/oz,D/oz,E/oz,F/oz,G/oz,H/oz,A/seeds,B/seeds,C/seeds,D/seeds,E/seeds,F/seeds,G/seeds,H/seeds,A Seed %%,B Seed %%,C Seed %%,D Seed %%,E Seed %%,F Seed %%,G Seed %%,H Seed %%"
selectedPacks2 |> List.iter (fun p -> fprintfn outFile "%d,%.2f,%.2f,%.2f,%.0f,%.2f,%.2f,%.2f,%.2f,%.0f,%.2f,%.2f,%.2f,%.2f,%.2f,%.2f,%.2f,%.2f,%.2f,%.2f,%.2f,%.2f,%.2f,%.2f,%.2f,%.2f,%.2f,%.2f,%.2f,%.2f,%.2f,%.2f,%.2f,%.2f" p.ComboId p.SeedsMeanDev p.SeedsStdDev p.AvgSeeds p.SeedsPerPack p.TotalCost p.GrossProfit p.NetProfit p.NetProfitPercent p.NumPackets p.SeedComponentsInPack[0].SupplyOz p.SeedComponentsInPack[1].SupplyOz p.SeedComponentsInPack[2].SupplyOz p.SeedComponentsInPack[3].SupplyOz p.SeedComponentsInPack[4].SupplyOz p.SeedComponentsInPack[5].SupplyOz p.SeedComponentsInPack[6].SupplyOz p.SeedComponentsInPack[7].SupplyOz p.SeedComponentsInPack[0].NumSeeds p.SeedComponentsInPack[1].NumSeeds p.SeedComponentsInPack[2].NumSeeds p.SeedComponentsInPack[3].NumSeeds p.SeedComponentsInPack[4].NumSeeds p.SeedComponentsInPack[5].NumSeeds p.SeedComponentsInPack[6].NumSeeds p.SeedComponentsInPack[7].NumSeeds p.SeedComponentsInPack[0].SeedPercentOfPack p.SeedComponentsInPack[1].SeedPercentOfPack p.SeedComponentsInPack[2].SeedPercentOfPack p.SeedComponentsInPack[3].SeedPercentOfPack p.SeedComponentsInPack[4].SeedPercentOfPack p.SeedComponentsInPack[5].SeedPercentOfPack p.SeedComponentsInPack[6].SeedPercentOfPack p.SeedComponentsInPack[7].SeedPercentOfPack)
//sortedPacksCombos |> List.iter (fun p -> fprintfn outFile "%A" p)
outFile.Flush()