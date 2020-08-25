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

    let private mprintln msg =
        Console.CursorLeft <- 0
        printf "                                           "
        Console.CursorLeft <- 0
        printfn "%s" msg

    type private ResultOfIndex =
    | Index of index: int
    | NotFound of index: int

    let rec private indexOf w state (sr: StreamReader) = 
        let state = state + 1
        if sr.Peek () = -1 
        then NotFound state
        else 
            if sr.ReadLine () = w 
            then Index state
            else indexOf w state sr
    
    let private _getCrc32bHash n =
        (n |> string 
        |> Text.ASCIIEncoding.ASCII.GetBytes
        |> Force.Crc32.Crc32Algorithm.Compute).ToString "x"
    
    let private getCrc32bHash n (sw: StreamWriter) =
        let r = _getCrc32bHash n
        sw.WriteLine r
        r

    type ResultOfGetUid =
    | Uid of uid: int
    | DictErr

    let rec private _getUid n id (sw: StreamWriter) =
        let r = getCrc32bHash n sw
        if r = id then 
            mprintln <| sprintf "[计算]%i" n
            Uid n
        else _getUid (n + 1) id sw

    let cacheFilePath = Environment.CurrentDirectory + "/hash_cache"
    
    let rec private getUidFromDict id =
        let stream = File.Open (cacheFilePath, FileMode.OpenOrCreate, FileAccess.ReadWrite)
        let sw = new StreamWriter (stream)
        let r =
            if stream.Length > 0L then
                let sr = new StreamReader (stream)
                match indexOf id 0 sr with
                | Index r -> 
                    mprintln <| sprintf "[字典]%i" r
                    if _getCrc32bHash r = id
                    then Uid r
                    else DictErr
                | NotFound i -> 
                    mprintln <| sprintf "[一]从%i开始扩展字典 " i
                    _getUid i id sw
            else _getUid 1 id sw
        match r with
        | Uid s -> 
            sw.Dispose ()
            stream.Dispose ()
            s
        | DictErr -> 
            mprintln <| sprintf "[一]字典中包含错误，已清除字典文件并重新计算。"
            sw.Dispose ()
            stream.Dispose ()
            File.Delete cacheFilePath
            getUidFromDict id

    let cache = Collections.Generic.Dictionary<string, string>()

    let getUid id index =
        mprintln <| sprintf "[%i]%s " (index + 1) id
        index
        |> uint32
        |> ProgressBar.update

        let fromDict = cache.ContainsKey id
        let result =
            if fromDict
            then cache.[id]
            else id |> getUidFromDict |> string
        if fromDict 
        then cache.Add (id, result)
        result
        
open utils

module DanmakuAnalyser =
    let private getDanmakuData parseUid i (node: DanmakusXml.D) =
        let args = node.P.Split ','
        XElement (
            XName.op_Implicit "danmaku",
            XText node.Value,
            getXAttribute "time"     <| getTime     args.[4],
            getXAttribute "uid"      <| (if parseUid then getUid args.[6] i else args.[6]),
            getXAttribute "playtime" <| getPlayTime args.[0],
            getXAttribute "color"    <| getHexColor args.[3],
            getXAttribute "mode"     <| getMode     args.[1],
            getXAttribute "pool"     <| getPool     args.[5],
            getXAttribute "fontsize"                args.[2]
        )

    let getDanmakus parseUid (xml: string) = 
        let ds = DanmakusXml.Parse xml
        
        printfn "共有%i条弹幕。" ds.Ds.Length
        if parseUid then
            printfn "开始解析 Uid ……"
            async {
                ds.Ds.Length
                |> uint32
                |> ProgressBar.show 
            } |> Async.Start

        let r = 
            ds.Chatid, 
            XDocument (
                XElement (
                    XName.op_Implicit "danmakus",
                    ds.Ds |> Array.mapi (getDanmakuData parseUid)
                )
            )

        if parseUid then
            ds.Ds.Length
            |> uint32
            |> ProgressBar.update
        
        r