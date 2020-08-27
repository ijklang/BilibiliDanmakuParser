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
        str.ToUpperInvariant ()
        |> (String.init (6 - str.Length) (fun _ -> "0")
            |> sprintf "#%s%s")

    let getPlayTime =
        Double.Parse
        >> TimeSpan.FromSeconds
        >> sprintf "%A"

    let unixStampBase = DateTime (1970, 1, 1, 0, 0, 0, DateTimeKind.Utc)

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

module private uidUtils =
    let private getCrc32bHash n =
        (n |> string
         |> Text.ASCIIEncoding.ASCII.GetBytes
         |> Force.Crc32.Crc32Algorithm.Compute
        ).ToString "x"

    type private FindResult =
    | Result of max: int64 * int * (int list * int64)[]
    | DictErr

    /// 1 -> 50001
    let private findUidsFromDictCache cache basei now total ids =
        let mutable uids = list.Empty
        let mutable hasErr = false
        let mutable now = now
        cache 
        |> Array.Parallel.iteri (fun i str ->
            match ids |> Array.tryFindIndex (fun (_, s) -> s = str) with
            | Some idi ->
                let indexs, ss = ids.[idi]
                let state = basei + int64 i
                if getCrc32bHash state = ss then
                    now <- now + 1
                    sprintf "[%i/%i] %s -> %i（位于 %A）" now total ss state indexs |> Console.WriteLine
                    uids <- (indexs, state) :: uids
                elif hasErr then ()
                else
                    sprintf "错误：%s -> %i" ss state |> Console.WriteLine
                    hasErr <- true
            | _ -> ()
        )
        if hasErr 
        then DictErr
        else
            let actualLen = 
                cache 
                |> Array.Parallel.choose (fun i -> if String.IsNullOrEmpty i then None else Some ())
                |> Array.length
                |> int64
            Result (basei + actualLen, now, List.toArray uids)

    let rec private readCache current (state: string[]) (sr: StreamReader) =
        if sr.Peek () = -1 || current = state.Length
        then state
        else
            state.[current] <- sr.ReadLine ()
            readCache (current + 1) state sr

    let rec private findUidsFromDict state sr now total ids uids =
        match ids with
        | [||] -> Result (state + 1L, now, uids)
        | _ ->
            let cache = readCache 0 (Array.create 5_0000 "") sr
            match findUidsFromDictCache cache (state + 1L) now total ids with
            | DictErr -> DictErr
            | Result (m, n, us) ->
                if sr.Peek () = -1
                then Result (m, n, Array.append uids us)
                else
                    let us', _ = Array.unzip us
                    let ids' = ids |> Array.Parallel.choose (fun ((i, _) as t) -> if us' |> Array.contains i then None else Some t )
                    findUidsFromDict (m - 1L) sr n total ids' (Array.append uids us)

    /// state 初始值是 1
    let rec private calUidFromId state id (sw: StreamWriter) =
        let hs =
            [| state .. state + 999L |]
            |> Array.Parallel.map getCrc32bHash
        hs 
        |> String.concat "\n" 
        |> sw.WriteLine
        match hs |> Array.tryFindIndex ((=) id) with
        | None -> calUidFromId (state + 1000L) id sw
        | Some i -> state + int64 i

    type private GetResult =
    | Continue of int list * int64
    | Break of string[]

    let private _getUids (cacheFileInfo: FileInfo) ids uids lastMax lastPosition now total =
        match ids with
        | [||] -> 
            uids
            |> Array.Parallel.map (fun (indexs, uid) -> 
                indexs 
                |> List.toArray 
                |> Array.Parallel.map (fun i -> i, string uid)
            ) |> Array.concat
            |> Array.sortBy (fun (i, _) -> i)
            |> Array.Parallel.map (fun (_, s) -> s)
            |> Break 
        | _ ->
            printfn "\n[一]扩充字典中……\n"
            let indexs, ss = ids.[0]

            let ws = cacheFileInfo.OpenWrite()
            ws.Position <- lastPosition
            let sw = new StreamWriter (ws)
            let uid = calUidFromId lastMax ss sw
            sw.Dispose ()

            sprintf "[%i/%i] %s -> %i（位于 %A）" now total ss uid indexs |> Console.WriteLine
            Continue <| (indexs, uid)

    let private cachePath = AppDomain.CurrentDomain.BaseDirectory + "/hash_cache"

    let private getNewIdAndUidArray uids uids' ids = 
        let uids'', _ = uids' |> Array.unzip
        let _, ids' = ids |> Array.Parallel.partition (fun (i, _) -> uids'' |> Array.contains i)
        ids', Array.append uids uids'

    let rec getUids (lastMax, lastPosition) now total uids ids =
        let cacheFile = FileInfo (cachePath)
        if not cacheFile.Exists
        then cacheFile.CreateText().Dispose()

        let rs = cacheFile.OpenRead()
        rs.Position <- if lastPosition = 0L then 0L else lastPosition + 1L
        let reader = new StreamReader (rs)
        let rrr = findUidsFromDict (lastMax - 1L) reader now total ids [||]
        let pos = reader.BaseStream.Position
        reader.Dispose ()

        match rrr with
        | DictErr ->
            cacheFile.Delete ()
            printfn "[一]字典中包含错误，已将其移除并准备重新生成。"
            printfn "    可能需要重新计算部分的 Uid 。"
            getUids (1L, 0L) now total uids ids
        | Result (m, n, uids') ->
            let ids', uids'' = getNewIdAndUidArray uids uids' ids
            let n = n + 1
            match _getUids cacheFile ids' uids'' m pos n total with
            | Break result -> result
            | Continue (indexs, uid) -> 
                let ids', uids'' = getNewIdAndUidArray uids'' [| indexs, uid |] ids'
                getUids (m, pos) n total uids'' ids'

open utils

module DanmakuAnalyser =
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

    let getDanmakus parseUid (xml: string) =
        let ds = DanmakusXml.Parse xml

        printfn "共有%i条弹幕。" ds.Ds.Length
        let ids =
            if parseUid then
                printfn "开始解析 Uid ……"
                let t =
                    ds.Ds
                    |> Array.Parallel.mapi (fun i j -> i, (j.P.Split ',').[6])
                ds.Ds 
                |> Array.Parallel.map (fun i -> (i.P.Split ',').[6])
                |> Array.distinct
                |> Array.Parallel.map (fun str -> 
                    let indexs = 
                        t |> Array.Parallel.choose (fun (i, s) -> 
                            if str = s 
                            then Some i
                            else None
                        ) |> Array.toList
                    indexs, str
                )
                |> (fun i -> uidUtils.getUids (1L, 0L) 0 i.Length [||] i)
            else ds.Ds |> Array.Parallel.map (fun i -> (i.P.Split ',').[6])

        ds.Chatid,
        XDocument (
            XElement (
                XName.op_Implicit "danmakus",
                (ds.Ds, ids) ||> Array.map2 getDanmakuData
            )
        )