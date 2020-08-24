namespace BilibiliDanmakuParser

open System
open System.Xml.Linq

type DanmakusXml = FSharp.Data.XmlProvider<"""
<i>
    <chatid>0</chatid>
    <d p="param">example1</d>
    <d p="param">example2</d>
</i>""">

type Danmaku = 
    { Time: string
      Content: string
      PlayTime: string
      Color: string
      Mode: string
      Pool: string
      FontSize: string }
      
    static member ToXElement x =
        XElement (
            XName.op_Implicit "danmaku",
            XAttribute (
                XName.op_Implicit "time",
                x.Time),
            XAttribute (
                XName.op_Implicit "content",
                x.Content),
            XAttribute (
                XName.op_Implicit "playtime",
                x.PlayTime),
            XAttribute (
                XName.op_Implicit "color",
                x.Color),
            XAttribute (
                XName.op_Implicit "mode",
                x.Mode),
            XAttribute (
                XName.op_Implicit "pool",
                x.Pool),
            XAttribute (
                XName.op_Implicit "fontsize",
                x.FontSize)
        )
    member x.ToXElement () = Danmaku.ToXElement x

module DanmakuAnalyser =
    module private utils = 
        let private getHexColor (color: string) = 
            let str = Convert.ToString (Int32.Parse color, 16)
            str.ToUpperInvariant () |> sprintf "#%s"
            
        let private getPlayTime = 
            Double.Parse
            >> TimeSpan.FromSeconds
            >> sprintf "%A"

        let private unixStampBase = DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc)
        
        let private getTime (time: string) = 
            unixStampBase + TimeSpan (Int64.Parse time * 1000_0000L)
            |> sprintf "%A"

        let private getPool =
            function
            | "0" -> "普通池"
            | "1" -> "字幕池"
            | "2" -> "特殊池"
            |  _  -> "?"

        let private getMode =
            function
            | "1" | "2" | "3" -> "普通弹幕"
            | "4" -> "底端弹幕"
            | "5" -> "顶端弹幕"
            | "6" -> "逆向弹幕"
            | "7" -> "精准定位"
            | "8" | "9" -> "高级弹幕"
            |  _  -> "?"

        let getDanmakuData (node: DanmakusXml.D) =
            let args = node.P.Split ','
            {
                Content  = node.Value
                PlayTime = getPlayTime args.[0]
                Mode     = getMode     args.[1]
                FontSize =             args.[2]
                Color    = getHexColor args.[3]
                Time     = getTime     args.[4]
                Pool     = getPool     args.[5]
            }

    let getDanmakuXElement (xml: string) = 
        let ds = DanmakusXml.Parse xml
        printfn "共有%i条弹幕。" ds.Ds.Length
        ds.Chatid, XDocument (
            XElement (
                XName.op_Implicit "danmakus",
                ds.Ds |> Array.map (utils.getDanmakuData >> Danmaku.ToXElement)
            )
        )