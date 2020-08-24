namespace BilibiliDanmakuParser

open System
open System.IO
open System.Xml.Linq

type DanmakusXml = FSharp.Data.XmlProvider<"""
<i>
    <chatid>0</chatid>
    <d p="param">example1</d>
    <d p="param">example2</d>
</i>""">

module private utils = 
    let getHexColor color = 
        let str = Convert.ToString (Int32.Parse color, 16)
        let getResult = 
            String.init (6 - str.Length) (fun _ -> "0")
            |> sprintf "#%s%s"
            
        str.ToUpperInvariant () 
        |> getResult
        
    let getPlayTime = 
        Double.Parse
        >> TimeSpan.FromSeconds
        >> sprintf "%A"

    let unixStampBase = DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc)
    
    let getTime time = 
        unixStampBase + TimeSpan (Int64.Parse time * 1000_0000L)
        |> sprintf "%A"

    let getPool =
        function
        | "0" -> "普通池"
        | "1" -> "字幕池"
        | "2" -> "特殊池"
        |  _  -> "?"

    let getMode =
        function
        | "1" | "2" | "3" -> "普通弹幕"
        | "4" -> "底端弹幕"
        | "5" -> "顶端弹幕"
        | "6" -> "逆向弹幕"
        | "7" -> "精准定位"
        | "8" | "9" -> "高级弹幕"
        |  _  -> "?"

    let getXAttribute name value = 
        XAttribute (XName.op_Implicit name, value)

    let private debugPrintn msg =
#if DEBUG
        Console.CursorLeft <- 0
        printf "                                           "
        Console.CursorLeft <- 0
        printfn "%s" msg
#endif
        ()

    type private ResultOfIndex =
    | Ok of index: int
    | NotFound of index: int

    let rec private indexOf w state (sr: StreamReader) = 
        let state = state + 1
        if sr.Peek () = -1 
        then NotFound state
        else 
            if sr.ReadLine () = w 
            then Ok state
            else indexOf w state sr

    let rec private getCrc32bHash n (sw: StreamWriter) =
        let r = 
            (n
            |> string 
            |> Text.ASCIIEncoding.ASCII.GetBytes
            |> Force.Crc32.Crc32Algorithm.Compute).ToString "x"
        sw.WriteLine r
        r

    let rec private _getUid n id (sw: StreamWriter) =
        let r = getCrc32bHash n sw
        if r = id then 
            debugPrintn <| sprintf "[计算]%i" n
            n
        else _getUid (n + 1) id sw

    let cacheFilePath = Environment.CurrentDirectory + "/hash_cache"

    let cache = Collections.Generic.Dictionary<string, int>()
    
    let getUid id index =
        debugPrintn <| sprintf "[%i]%s " (index + 1) id
        index
        |> uint32
        |> ProgressBar.update
        if cache.ContainsKey id then 
            cache.[id] |> fun i -> debugPrintn <| sprintf "[缓存]%i" i; i
        else
            let r =
                use stream = File.Open (cacheFilePath, FileMode.OpenOrCreate, FileAccess.ReadWrite)
                use sw = new StreamWriter (stream)
                if stream.Length > 0L then
                    let sr = new StreamReader (stream)
                    match indexOf id 0 sr with
                    | Ok r -> debugPrintn <| sprintf "[字典]%i" r; r
                    | NotFound i -> 
                        debugPrintn <| sprintf "[一]从%i开始扩展字典 " i
                        _getUid i id sw
                else _getUid 1 id sw
            cache.Add (id, r)
            r

open utils

module DanmakuAnalyser =
    let private getDanmakuData i (node: DanmakusXml.D) =
        let args = node.P.Split ','
        XElement (
            XName.op_Implicit "danmaku",
            XText node.Value,
            getXAttribute "time"     <| getTime     args.[4],
            getXAttribute "uid"      <| getUid      args.[6] i,
            getXAttribute "playtime" <| getPlayTime args.[0],
            getXAttribute "color"    <| getHexColor args.[3],
            getXAttribute "mode"     <| getMode     args.[1],
            getXAttribute "pool"     <| getPool     args.[5],
            getXAttribute "fontsize"                args.[2]
        )

    let getDanmakus (xml: string) = 
        let ds = DanmakusXml.Parse xml
        printfn "共有%i条弹幕。" ds.Ds.Length
        printfn "开始解析 Uid ……"
        async {
            ds.Ds.Length
            |> uint32
            |> ProgressBar.show 
        } |> Async.Start

        let r = ds.Chatid, XDocument (
                    XElement (
                        XName.op_Implicit "danmakus",
                        ds.Ds |> Array.mapi getDanmakuData
                    )
                )
        ds.Ds.Length
        |> uint32
        |> ProgressBar.update
        r
