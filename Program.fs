module BilibiliDanmakuParser.Main

open System
open System.IO
open System.IO.Compression
open System.Net.Http
open System.Xml.Linq

let printlnInRed msg =
    Console.ForegroundColor <- ConsoleColor.Red
    printfn "%s" msg
    Console.ForegroundColor <- ConsoleColor.Gray

let rec saveResult fileIndex (cid, xdoc: XDocument) =
    let indexText =
        if fileIndex = 0
        then String.Empty
        else sprintf "(%i)" fileIndex
    let path =
        sprintf "%s%cresult_%i%s.xml"
            Environment.CurrentDirectory
            Path.DirectorySeparatorChar
            cid
            indexText

    if File.Exists path
    then saveResult (fileIndex + 1) (cid, xdoc)
    else
        let stream = File.Create path
        xdoc.Save (stream, SaveOptions.None)
        path

let fromXml needParseUid xml =
    xml
    |> DanmakuAnalyser.getDanmakus needParseUid
    |> saveResult 0
    |> printfn "\n分析完成，已保存至%s。"

let fromCid needParseUid cid =
    printfn "正在获取该视频的弹幕列表……"
    use client = new HttpClient ()
    let uri = sprintf "https://comment.bilibili.com/%i.xml" cid
    async {
        let! bytes = client.GetByteArrayAsync uri |> Async.AwaitTask
        use memoryStream  = new MemoryStream (bytes)
        use deflateStream = new DeflateStream (memoryStream, CompressionMode.Decompress)
        use streamReader  = new StreamReader (deflateStream, Text.Encoding.UTF8)
        let! result = streamReader.ReadToEndAsync() |> Async.AwaitTask
        printfn "获取完成。\n"
        fromXml needParseUid result
    } |> Async.RunSynchronously

module FromVideoPart =
    type private PartsJson = FSharp.Data.JsonProvider<"""
    {
        "code":0,
        "message":"err",
        "data":[
            {
                "cid":100000000000,
                "page":1,
                "part":"example"
            }
        ]
    }
    """>

    let private printPartInfos (data: PartsJson.Datum[]) =
        printfn "该稿件共有%i个视频。" data.Length
        for i in data do
            printfn "[%i]%s" i.Page i.Part

    let rec private readSelectedPartIndex (data: PartsJson.Datum[]) =
        printPartInfos data
        printfn "\n请选择一个视频："

        let input = Console.ReadLine ()
        let mutable result = 0
        if Int32.TryParse (input, &result) && result > 0 && result <= data.Length then
            printfn "\n已选择 p%i ：%s 。" result data.[result - 1].Part
            result
        else
            data.Length
            |> sprintf "输入的内容 i 应在 [1, %i] 内。\n"
            |> printlnInRed
            readSelectedPartIndex data

    let private cli needParseUid (uri: string) =
        use client = new HttpClient ()
        async {
            let! partsInfo = client.GetStringAsync uri |> Async.AwaitTask
            let parts = PartsJson.Parse partsInfo

            if parts.Code = 0 then
                let selected =
                    if parts.Data.Length = 1
                    then 1
                    else readSelectedPartIndex parts.Data
                fromCid needParseUid
                    parts.Data.[selected - 1].Cid

            else sprintf "服务器返回 { %i }：%s。"
                    parts.Code
                    parts.Message
                 |> printlnInRed
        } |> Async.RunSynchronously

    let fromAvid needParseUid aid =
        aid
        |> Int64.Parse
        |> sprintf "https://api.bilibili.com/x/player/pagelist?aid=%i"
        |> cli needParseUid

    let fromBvid needParseUid bvid =
        bvid
        |> sprintf "https://api.bilibili.com/x/player/pagelist?bvid=%s"
        |> cli needParseUid

let validCmd =
    [
        "-av" ; "--FromAVId"       ; "-avu"; "--FromAVIdu"
        "-bv" ; "--FromBVId"       ; "-bvu"; "--FromBVIdu"
        "-c"  ; "--FromCId"        ; "-cu" ; "--FromCIdu"
        "-f"  ; "--FromDanmakuFile"; "-fu" ; "--FromDanmakuFileu"
    ]

[<EntryPoint>]
let main argv =
    try
        Console.CancelKeyPress.Add
            (fun _ -> printfn "\n程序中止。")
        if argv.Length > 0 then
            argv
            |> Array.fold
                (fun state s ->
                    sprintf "%s \"%s\"" state s)
                "当前命令："
            |> printfn "%s\n"
        let stopwatch = Diagnostics.Stopwatch ()
        stopwatch.Start ()

        match argv with
        | [| cmd; arg |] when validCmd |> List.contains cmd ->
            let parseUid = cmd.[cmd.Length - 1] = 'u'
            match cmd with
            | "-av" | "--FromAVId" | "-avu" | "--FromAVIdu" ->
                FromVideoPart.fromAvid parseUid arg
            | "-bv" | "--FromBVId" | "-bvu" | "--FromBVIdu" ->
                FromVideoPart.fromBvid parseUid arg
            | "-c" | "--FromCId" | "-cu" | "--FromCIdu" ->
                arg
                |> Int64.Parse
                |> fromCid parseUid
            | "-f"  | "--FromDanmakuFile" | "-fu" | "--FromDanmakuFileu" ->
                arg
                |> File.ReadAllText
                |> fromXml parseUid
            | _ -> ()
        | _ ->
            if argv.Length > 0
            then printlnInRed "命令不正确。"
            printfn "
获取Bilibili的弹幕数据。

用法：
    BilibiliDanmakuParser 命令 参数

命令及参数说明：
    -av, --FromAVId
    -avu, --FromAVIdu
        参数：AV号
    -bv, --FromBVId
    -bvu, --FromBVIdu
        参数：BV号（包含前缀“BV”）
        获取特定稿件下某视频的弹幕列表。

    -c, --FromCId
    -cu, --FromCIdu
        参数：某视频的编号（即“cid”）
        获取特定BV号下的弹幕列表。

    -f, --FromDanmakuFile
    -fu, --FromDanmakuFileu
        参数：以XML格式保存的弹幕文件路径
        分析指定的弹幕文件。

        以“u”结尾的命令生成的文件会解密 Uid，但由于程序是通过
    暴力枚举来计算 Uid 的，运行速度会非常很慢，同时会占用部分
    磁盘空间来储存解密字典（大约 6GiB）。"

        stopwatch.Stop ()
        printfn "\n本次运行耗时 %A 。" stopwatch.Elapsed
        0
    with ex ->
        sprintf "发生异常。\n%A" ex
        |> printlnInRed
        ex.HResult