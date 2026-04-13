open System
open System.Reflection
open System.Linq
open System.IO

[<EntryPoint>]
let main argv =
    let testDll = @"C:\temp\Test.dll"
    let repoDll = @"C:\temp\Repository.dll"

    let ignoreMethods = [| "ToString"; "Equals"; "GetHashCode"; "GetType" |]

    let testAssembly = Assembly.LoadFile(testDll)
    let repoAssembly = Assembly.LoadFile(repoDll)

    let repoMethods =
        repoAssembly.GetTypes()
        |> Seq.filter (fun t -> t.Name.EndsWith("Repository") && (not <| t.IsInterface))
        |> Seq.map (fun t -> t.GetMethods(BindingFlags.Instance ||| BindingFlags.Public) |> Seq.filter (fun m -> not <| ignoreMethods.Contains(m.Name)))
        |> Seq.concat

    let canMethods =
        testAssembly.GetTypes()
        |> Seq.map (fun t -> t.GetMethods(BindingFlags.Instance ||| BindingFlags.Public) |> Seq.filter (fun m -> not <| ignoreMethods.Contains(m.Name)))
        |> Seq.concat
        |> Seq.filter (fun m -> m.Name.StartsWith("Can_") || m.Name.EndsWith("_Can"))
    
    let repoLookup = repoMethods |> Seq.map (fun m -> m.DeclaringType.Name, m.Name) |> List.ofSeq
    let methodLookup = canMethods |> Seq.map (fun m -> m.DeclaringType.Name.Replace("Tests", ""), m.Name.Replace("Can_", "").Replace("_Can", "")) |> List.ofSeq

    let set1 = repoLookup |> Set.ofList
    let set2 = methodLookup |> Set.ofList

    let missingMethods = set1 - set2

    missingMethods |> Seq.iter (fun (repo, meth) -> printfn "%s.%s" repo meth)

    printfn "Done."
    Console.ReadLine() |> ignore
    0 // return an integer exit code
