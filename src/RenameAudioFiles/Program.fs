// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
open System
open System.IO

[<EntryPoint>]
let main argv = 
    let renameFile (file : FileInfo) newName =
        if not <| File.Exists newName then
            let dateString = file.LastWriteTime.ToString("yyyy-MM-dd HH:mm:ss")
            printfn "[%s]  %14s --> %14s" dateString file.Name newName
            file.MoveTo(newName)

    let letters = [| 'A'..'Z' |];
    Directory.EnumerateFiles(".", "*.WMA", SearchOption.TopDirectoryOnly)
    |> Seq.map (fun path -> new FileInfo(path))
    //|> Seq.filter (fun file -> file.Name.ToUpper().StartsWith("WS"))
    |> Seq.groupBy (fun file -> file.LastWriteTime.Date)
    |> Seq.map (fun (date, files) -> date, files |> Seq.sortBy (fun f -> f.LastWriteTime))
    |> Seq.iter (fun (date, files) ->
            if Seq.length files > 1 then
                files
                |> Seq.iteri (fun index file ->
                    let newName = String.Format(@".\{0:yyyy-MM-dd}{1}.WMA", date, letters.[index])
                    renameFile file newName
                    )
            else
                let file = files |> Seq.exactlyOne
                let newName = String.Format(@".\{0:yyyy-MM-dd}.WMA", date)
                renameFile file newName
        )
    0 // return an integer exit code