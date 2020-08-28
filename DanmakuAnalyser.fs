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

module private uidUtils =
    let private getCrc32bHash num =
        num
        |> string
        |> Text.ASCIIEncoding.ASCII.GetBytes
        |> Force.Crc32.Crc32Algorithm.Compute
        |> (fun s -> s.ToString "x")

    type private FindResult =
        | Result of max: int64 * current: int * newUids: (int list * int64) list
        | DictErr

    /// 1 -> 50001
    let private findUidsFromDictCache cache basei (now, total) ids =
        let mutable uids = list.Empty
        let mutable hasErr = false
        let mutable now = now

        cache
        |> Array.Parallel.iteri (fun i str ->
            match ids |> Array.tryFindIndex (fun (_, s) -> s = str) with
            | Some idi ->
                let indexs, id = ids.[idi]
                let state = basei + int64 i
                if getCrc32bHash state = id then
                    now <- now + 1
                    uids <- (indexs, state) :: uids
                    sprintf "[%i/%i] %s -> %i（位于 %A）" now total id state indexs 
                    |> Console.WriteLine
                elif hasErr then ()
                else
                    sprintf "错误：%s -> %i" id state 
                    |> Console.WriteLine
                    hasErr <- true
            | _ -> ()
        )

        if hasErr
        then DictErr
        else
            let actualLength =
                cache
                |> Array.Parallel.choose 
                    (fun i -> 
                        if String.IsNullOrEmpty i 
                        then None 
                        else Some ())
                |> Array.length
                |> int64
            Result (basei + actualLength, now, uids)

    let rec private readCache current (state: string[]) (reader: StreamReader) =
        if reader.Peek () = -1 || current = state.Length
        then state
        else
            state.[current] <- reader.ReadLine ()
            readCache (current + 1) state reader

    let rec private findUidsFromDict state reader (now, total) ids uids =
        match ids with
        | [||] -> Result (state + 1L, now, uids)
        | _ ->
            let cache = readCache 0 (Array.create 5_0000 "") reader
            match findUidsFromDictCache cache (state + 1L) (now, total) ids with
            | DictErr -> DictErr
            | Result (m, n, us) ->
                if reader.Peek () = -1
                then Result (m, n, us @ uids)
                else
                    let us', _ = List.unzip us
                    let ids' = 
                        ids 
                        |> Array.Parallel.choose 
                            (fun ((i, _) as t) -> 
                                if us' |> List.contains i 
                                then None 
                                else Some t)
                    findUidsFromDict (m - 1L) reader (n, total) ids' (us @ uids)

    /// state 初始值是 1
    let rec private calUidAndEnlargeDict state id (sw: StreamWriter) =
        let hs =
            [| state .. state + 999L |]
            |> Array.Parallel.map getCrc32bHash
        hs
        |> String.concat "\n"
        |> sw.WriteLine
        match hs |> Array.tryFindIndex ((=) id) with
        | None -> calUidAndEnlargeDict (state + 1000L) id sw
        | Some i -> state + int64 i

    type private GetResult =
        | Continue of int list * int64
        | Break of string[]

    let private _getUids (cacheFileInfo: FileInfo) ids uids lastMax lastPosition now total =
        match ids with
        | [||] ->
            uids
            |> List.toArray
            |> Array.Parallel.map
                (fun (indexs, uid) ->
                    indexs
                    |> List.toArray
                    |> Array.Parallel.map
                        (fun i -> i, string uid))
            |> Array.concat
            |> Array.sortBy
                (fun (i, _) -> i)
            |> Array.Parallel.map
                (fun (_, s) -> s)
            |> Break
        | _ ->
            printfn "\n[一]扩充字典中……\n"
            let indexs, ss = ids.[0]

            let ws = cacheFileInfo.OpenWrite()
            ws.Position <- lastPosition
            let sw = new StreamWriter (ws)
            let uid = calUidAndEnlargeDict lastMax ss sw
            sw.Dispose ()

            sprintf "[%i/%i] %s -> %i（位于 %A）" now total ss uid indexs
            |> Console.WriteLine
            Continue (indexs, uid)

    let private dictPath = AppDomain.CurrentDomain.BaseDirectory + "/hash_cache"

    let private removeSameIndexedIds newUids ids =
        let indexs, _ = newUids |> List.unzip
        ids
        |> Array.Parallel.choose
            (fun ((i, _) as t) ->
                if indexs |> List.contains i
                then None
                else Some t)

    let rec getUids (lastReadedMax, lastReadedPosition) (current, total) uids ids =
        let cacheFile = FileInfo (dictPath)
        if not cacheFile.Exists
        then cacheFile.CreateText().Dispose ()

        let readStream = cacheFile.OpenRead ()
        readStream.Position <-
            if lastReadedPosition = 0L
            then 0L
            else lastReadedPosition + 1L
        let streamReader = new StreamReader (readStream)
        let foundResult = findUidsFromDict (lastReadedMax - 1L) streamReader (current, total) ids []
        let lastReadedPosition' = streamReader.BaseStream.Position
        streamReader.Dispose ()

        match foundResult with
        | DictErr ->
            cacheFile.Delete ()
            printfn "
[一]字典中包含错误，已将其移除并准备重新生成。
    可能需要重新计算部分的 Uid 。"
            getUids (1L, 0L) (current, total) uids ids
        | Result (m, n, uids') ->
            let ids' = removeSameIndexedIds uids' ids
            let n = n + 1
            match _getUids cacheFile ids' (uids' @ uids) m lastReadedPosition' n total with
            | Continue (indexs, uid) ->
                let uids' = [ indexs, uid ]
                let ids' = removeSameIndexedIds uids' ids'
                getUids (m, lastReadedPosition') (n, total) (uids' @ uids) ids'
            | Break result -> result

module DanmakuAnalyser =
    let private getHexColor color =
        let str = Convert.ToString (Int32.Parse color, 16)
        str.ToUpperInvariant ()
        |> (String.init (6 - str.Length) (fun _ -> "0")
            |> sprintf "#%s%s")

    let private getPlayTime time =
        time
        |>Double.Parse
        |> TimeSpan.FromSeconds
        |> string

    let private unixStampBase =
        DateTime (1970, 1, 1, 0, 0, 0, DateTimeKind.Utc)
    let private getTime time =
        let time =
            time + "0000000"
            |> Int64.Parse
            |> TimeSpan
        unixStampBase + time
        |> string

    let private getPool =
        function
        | "0" -> "普通池"
        | "1" -> "字幕池"
        | "2" -> "特殊池"
        | _ -> "未知"

    let private getMode =
        function
        | "1" | "2" | "3" -> "普通弹幕"
        | "4" -> "底端弹幕"
        | "5" -> "顶端弹幕"
        | "6" -> "逆向弹幕"
        | "7" -> "精准定位"
        | "8" | "9" -> "高级弹幕"
        | _ -> "未知"

    let private getXAttribute name value =
        XAttribute (XName.op_Implicit name, value)

    let private getDanmakuData (node: DanmakusXml.D) uid =
        let args = node.P.Split ','
        XElement (
            XName.op_Implicit "danmaku",
            XText node.Value,
            getXAttribute "time"     <| getTime     args.[4],
            getXAttribute "uid"      <|             uid,
            getXAttribute "playtime" <| getPlayTime args.[0],
            getXAttribute "color"    <| getHexColor args.[3],
            getXAttribute "mode"     <| getMode     args.[1],
            getXAttribute "pool"     <| getPool     args.[5],
            getXAttribute "fontsize"                args.[2]
        )

    let private getUids parseUid (danmakus: DanmakusXml.D[]) =
        let ids =
            danmakus
            |> Array.Parallel.map
                (fun i -> (i.P.Split ',').[6])

        if parseUid then
            printfn "开始解析 Uid ……"
            let indexedIds =
                ids
                |> Array.Parallel.mapi
                    (fun i s -> i, s)

            ids
            |> Array.distinct
            |> Array.Parallel.map
                (fun str ->
                    let indexs =
                        indexedIds
                        |> Array.Parallel.choose
                            (fun (i, s) ->
                                if str = s
                                then Some i
                                else None)
                        |> Array.toList
                    indexs, str)
            |> (fun r ->
                    uidUtils.getUids (1L, 0L) (0, r.Length) [] r)
        else ids

    let getDanmakus parseUid (xml: string) =
        let ds = DanmakusXml.Parse xml

        printfn "共有%i条弹幕。" ds.Ds.Length
        let ids = getUids parseUid ds.Ds

        ds.Chatid,
        XDocument (
            XElement (
                XName.op_Implicit "danmakus",
                (ds.Ds, ids) ||> Array.map2 getDanmakuData
            )
        )