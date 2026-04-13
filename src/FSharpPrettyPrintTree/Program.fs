// Learn more about F# at http://fsharp.org

open System
open System.Text

type TextNode = { Text : string; Children : TextNode[] }

[<EntryPoint>]
let main argv =
    // Not in pure functional style but oh well.
    let printPretty (node : TextNode) =
        let sb = new StringBuilder()
        let rec printPrettyRec (node : TextNode) (indent : string) (last : bool) =
            sb.Append(indent) |> ignore
            let newIndent =
                if last then
                    sb.Append("\\-") |> ignore        // Side-effect. Fun.
                    indent + "  "
                else
                    sb.Append("|-") |> ignore
                    indent + "| "
            sb.AppendLine(node.Text) |> ignore
            let lastChildIndex = node.Children.Length - 1 
            for i in 0..lastChildIndex do
                printPrettyRec node.Children.[i] newIndent (i = lastChildIndex)
        printPrettyRec node "" true
        sb.ToString()

    let testTree = { Text = "ROOT"; Children = [| { Text = "A"; Children = [||] }; { Text = "B"; Children = [||] }; { Text = "C"; Children = [| { Text = "D"; Children = [| { Text = "E"; Children = [||] } |] }; { Text = "F"; Children = [||] } |] } |] }

    printfn "%s" (printPretty testTree)
    Console.ReadKey() |> ignore
    0 // return an integer exit code
