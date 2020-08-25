﻿open System
open System.IO
open System.IO.Compression
open System.Net.Http
open System.Xml.Linq

let printlnInRed msg =
    Console.ForegroundColor <- ConsoleColor.Red
    printfn "%s" msg
    Console.ForegroundColor <- ConsoleColor.Gray

let rec saveResult n (cid, xdoc: XDocument) =
    let path = sprintf "%s/result_%i_(%i).xml" Environment.CurrentDirectory cid n
    if File.Exists path 
    then saveResult (n + 1) (cid, xdoc)
    else 
        let stream = File.Create path
        xdoc.Save (stream, SaveOptions.None)
        path

let fromXml parseUid xml =
    xml
    |> BilibiliDanmakuParser.DanmakuAnalyser.getDanmakus parseUid
    |> saveResult 0
    |> (fun x -> 
            Console.CursorLeft <- 0
            printf " 　　　　　　　　　　　　　　　　　　　　　　　　　　　　　"
            Console.CursorLeft <- 0
            Console.CursorVisible <- true 
            x
    )
    |> printfn "\n分析完成，已保存至%s。"
    

let fromCid parseUid cid = 
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
        fromXml parseUid result
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
        for i in data do 
            printfn "[%i]%s" i.Page i.Part

    let rec private readPartIndex max = 
        printfn "\n请选择一个视频："
        let input = Console.ReadLine ()
        let mutable r = 0
        if Int32.TryParse (input, &r) && r > 0 && r <= max 
        then r
        else 
            printlnInRed "输入的内容错误。"
            readPartIndex max

    let private cli parseUid (uri: string) =
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
                fromCid parseUid json.Data.[selectedPart - 1].Cid
            else printlnInRed <| sprintf "服务器返回错误代码%i：%s。" json.Code json.Message
        } |> Async.RunSynchronously

    let cliAvid parseUid =
        cli parseUid << sprintf "https://api.bilibili.com/x/player/pagelist?aid=%i"

    let cliBvid parseUid =
        cli parseUid << sprintf "https://api.bilibili.com/x/player/pagelist?bvid=%s"

let showHelp () =
    printfn "获取Bilibili的弹幕数据。"
    printfn ""
    printfn "用法："
    printfn "    BilibiliDanmakuParser 命令 参数"
    printfn "命令及参数说明："
    printfn "    -av, --FromAVId"
    printfn "    -avu, --FromAVIdu"
    printfn "        参数：AV号"
    printfn "    -bv, --FromBVId"
    printfn "    -bvu, --FromBVIdu"
    printfn "        参数：BV号（包含前缀“BV”）"
    printfn "        获取特定稿件下某视频的弹幕列表。"
    printfn "    -c, --FromCId"
    printfn "    -cu, --FromCIdu"
    printfn "        参数：某视频的编号（即“cid”）"
    printfn "        获取特定BV号下的弹幕列表。"
    printfn "    -f, --FromDanmakuFile"
    printfn "    -fu, --FromDanmakuFileu"
    printfn "        参数：以XML格式保存的弹幕文件路径"
    printfn "        分析指定的弹幕文件。"
    printfn "    以“u”结尾的命令生成的文件会解密 Uid，但由于程序用的是暴力枚举 Uid ，"
    printfn "运行速度很慢，同时会占用部分磁盘空间（大约 6GiB）。"

let validCmd = 
    [ 
        "-av"; "--FromAVId"
        "-avu"; "--FromAVIdu"
        "-bv"; "--FromBVId"
        "-bvu"; "--FromBVIdu"
        "-c"; "--FromCId"
        "-cu"; "--FromCIdu"
        "-f"; "--FromDanmakuFile"
        "-fu"; "--FromDanmakuFileu"
    ]

[<EntryPoint>]
let main argv =
    try
        Console.CancelKeyPress.Add(fun _ ->
            Console.CursorVisible <- true
            printfn "\n程序中止。"
        )
        match argv with
        | [| cmd; arg |] when validCmd |> List.contains cmd ->
            match cmd with
            | "-av" | "--FromAVId" ->
                arg
                |> Int32.Parse
                |> FromVideoPart.cliAvid false
            
            | "-avu" | "--FromAVIdu" ->
                arg
                |> Int32.Parse
                |> FromVideoPart.cliAvid true

            | "-bv" | "--FromBVId" ->
                FromVideoPart.cliBvid false arg

            | "-bvu" | "--FromBVIdu" ->
                FromVideoPart.cliBvid true arg

            | "-c" | "--FromCId" -> 
                arg
                |> Int32.Parse
                |> fromCid false

            | "-cu" | "--FromCIdu" -> 
                arg
                |> Int32.Parse
                |> fromCid true

            | "-f" | "--FromDanmakuFile" -> 
                arg
                |> File.ReadAllText
                |> fromXml false
                
            | "-fu" | "--FromDanmakuFileu" -> 
                arg
                |> File.ReadAllText
                |> fromXml true

            | _ -> ()

        | [||] -> showHelp ()

        | _ -> 
            argv 
            |> String.concat " "
            |> sprintf "参数“%s”不正确。"
            |> printlnInRed
            showHelp ()
        0
    with ex -> 
        printlnInRed <| sprintf "发生异常。\n%A" ex
        ex.HResult
