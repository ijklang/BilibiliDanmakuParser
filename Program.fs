open System
open System.IO
open System.IO.Compression
open System.Net.Http
open System.Xml.Linq

let rec saveResult n (cid, xdoc: XDocument) =
    let path = sprintf "%s/result_%i_(%i).xml" Environment.CurrentDirectory cid n
    if File.Exists path 
    then saveResult (n + 1) (cid, xdoc)
    else 
        let stream = File.Create path
        xdoc.Save (stream, SaveOptions.None)
        path

let fromXml =
    BilibiliDanmakuParser.DanmakuAnalyser.getDanmakuXElement
    >> saveResult 0
    >> printfn "分析完成，已保存至%s。"

let fromCid cid = 
    printfn "正在获取弹幕列表……"
    use client = new HttpClient ()
    let uri = Uri <| sprintf "https://comment.bilibili.com/%i.xml" cid
    async {
        let! bytes = client.GetByteArrayAsync uri |> Async.AwaitTask
        use memoryStream  = new MemoryStream (bytes)
        use deflateStream = new DeflateStream (memoryStream, CompressionMode.Decompress)
        use streamReader  = new StreamReader (deflateStream, Text.Encoding.UTF8)
        let! result = streamReader.ReadToEndAsync() |> Async.AwaitTask
        printfn "获取完毕。\n"
        fromXml result
    } |> Async.RunSynchronously

module FromVideoPart =
    type private PartsJson = FSharp.Data.JsonProvider<"""
    {
        "code":0,
        "message":"err",
        "data":[
            {
                "cid":0,
                "page":1,
                "part":"example"
            }
        ]
    }
    """>

    let private showParts (data: PartsJson.Datum seq) =
        for i in data do printfn "[%i]%s" i.Page i.Part

    let rec private readPartIndex max = 
        printfn "\n请选择一个视频："
        let input = Console.ReadLine ()
        let mutable r = 0
        if Int32.TryParse (input, &r) && r > 0 && r <= max 
        then r
        else 
            Console.ForegroundColor <- ConsoleColor.Red
            printfn "输入的内容错误。"
            Console.ForegroundColor <- ConsoleColor.Gray
            readPartIndex max

    let private cli (uri: string) =
        use client = new HttpClient ()
        async {
            let! str = client.GetStringAsync uri |> Async.AwaitTask
            let json = PartsJson.Parse str
            if json.Code = 0 then
                printfn "该稿件共有%i个视频。" json.Data.Length
                showParts json.Data
                let selectedPart =
                    if json.Data.Length = 1 
                    then 1
                    else readPartIndex json.Data.Length
                printfn "\n已选择 p%i ：%s 。" selectedPart json.Data.[selectedPart - 1].Part
                fromCid json.Data.[selectedPart - 1].Cid
            else
                Console.ForegroundColor <- ConsoleColor.Red
                printfn "服务器返回错误代码%i：%s。" json.Code json.Message
        } |> Async.RunSynchronously

    let cliAvid =
        cli << sprintf "https://api.bilibili.com/x/player/pagelist?aid=%i"

    let cliBvid =
        cli << sprintf "https://api.bilibili.com/x/player/pagelist?bvid=%s"

let showHelp () =
    printfn "用法：BilibiliDanmakuParser 命令 参数"
    printfn "获取/分析Bilibili的弹幕数据。"
    printfn "命令："
    printfn "  -av, --FromAVId"
    printfn "    参数：AV号"
    printfn "  -bv, --FromBVId"
    printfn "    参数：BV号（包含前缀“BV”）"
    printfn "    获取特定稿件下某视频的弹幕列表。"
    printfn "  -c, --FromCId"
    printfn "    参数：某视频的编号（即“cid”）"
    printfn "    获取特定BV号下的弹幕列表。"
    printfn "  -f, --FromDanmakuFile"
    printfn "    参数：以XML格式保存的弹幕文件路径"
    printfn "    分析指定的弹幕文件。"

let validCmd = 
    [ 
        "-av"; "--FromAVId"
        "-bv"; "--FromBVId"
        "-c"; "--FromCId"
        "-f"; "--FromDanmakuFile"
    ]

[<EntryPoint>]
let main argv =
    try
        match argv with
        | [| cmd; arg |] when validCmd |> List.contains cmd -> 
            match cmd with
            | "-av" | "--FromAVId" ->
                arg
                |> Int32.Parse
                |> FromVideoPart.cliAvid

            | "-bv" | "--FromBVId" ->
                FromVideoPart.cliBvid arg

            | "-c" | "--FromCId" -> 
                arg
                |> Int32.Parse
                |> fromCid

            | "-f" | "--FromDanmakuFile" -> 
                arg
                |> File.ReadAllText
                |> fromXml

            | _ -> ()

        | [||] -> showHelp ()

        | _ -> 
            Console.ForegroundColor <- ConsoleColor.Red
            argv 
            |> String.concat " "
            |> printfn "参数“%s”不正确。"
            Console.ForegroundColor <- ConsoleColor.Gray
            showHelp ()
        0
    with ex -> 
        Console.ForegroundColor <- ConsoleColor.Red
        printfn "发生异常。"
        printfn "%A" ex
        1
