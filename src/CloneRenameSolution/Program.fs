// Learn more about F# at http://fsharp.org

open System
open System.IO
open System.Linq

[<EntryPoint>]
let main argv =
    let printUsage() = printfn @"Usage: crs ""source directory"" ""dest directory"" ""source text"" ""replacement text"""
    if argv.Length < 4 then
        printUsage()
        1
    else
        let fileFilter = ["*.cs"; "*.csproj"]
        let ignoreDirectories = ["node_modules"; "bin"]
        let sourceDir = argv.[0]
        let destDir = argv.[1]
        let sourceText = argv.[2]
        let repText = argv.[3]

        if (not <| Directory.Exists(sourceDir)) then
            printUsage()
            printfn "Source directory %s doesn't exist!" sourceDir
            1
        elif (Directory.Exists(destDir)) then
            printUsage()
            printfn "Destination directory %s already exists!" destDir
            1
        else
            let sourceDirInfo = new DirectoryInfo(sourceDir)
            let destDirInfo = Directory.CreateDirectory(destDir)
            sourceDirInfo.EnumerateDirectories("*", new EnumerationOptions(RecurseSubdirectories = true))
            |> Seq.filter (fun dir -> not <| ignoreDirectories.Contains(dir.Name))
            |> Seq.map (fun dir -> Path.GetRelativePath(sourceDir, dir.FullName))
            |> Seq.iter (fun newDir -> Directory.CreateDirectory(Path.Combine(destDir, newDir.Replace(sourceText, repText))) |> ignore)
            
            Directory.EnumerateFiles(sourceDir, "*.*", new EnumerationOptions(RecurseSubdirectories = true))
            |> Seq.filter (fun file -> not <| Path.GetFullPath(file).Split('\\').Any(fun p -> ignoreDirectories.Contains(p)))
            |> Seq.iter (fun file ->
                            let dir = Path.GetDirectoryName(file)
                            let relPath = Path.GetRelativePath(sourceDir, dir)
                            let newPath = Path.Combine(destDir, relPath)
                            let newFileName = Path.Combine(newPath, Path.GetFileName(file)).Replace(sourceText, repText)
                            File.Copy(file, newFileName)

                            let newFileContents = File.ReadAllText(newFileName).Replace(sourceText, repText)
                            File.WriteAllText(newFileName, newFileContents)
                        )
            printfn "Done."
            Console.ReadKey |> ignore
            0
