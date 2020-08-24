namespace BilibiliDanmakuParser

open System
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

open utils

module DanmakuAnalyser =
    let private getDanmakuData (node: DanmakusXml.D) =
        let args = node.P.Split ','
        XElement (
            XName.op_Implicit "danmaku",
            XText node.Value,
            getXAttribute "time"     <| getTime     args.[4],
            getXAttribute "playtime" <| getPlayTime args.[0],
            getXAttribute "color"    <| getHexColor args.[3],
            getXAttribute "mode"     <| getMode     args.[1],
            getXAttribute "pool"     <| getPool     args.[5],
            getXAttribute "fontsize"                args.[2]
        )

    let getDanmakus (xml: string) = 
        let ds = DanmakusXml.Parse xml
        printfn "共有%i条弹幕。" ds.Ds.Length
        ds.Chatid, XDocument (
            XElement (
                XName.op_Implicit "danmakus",
                ds.Ds |> Array.map (getDanmakuData)
            )
        )