module BilibiliDanmakuParser.ProgressBar

type Console = System.Console

type private ProgressBarData = {
    Max: uint32
    Current: uint32
    GraphIndex: byte
}

let private getFloatValue a b = float a / float b * 100.

let private drawModel model =
    Console.CursorLeft <- 0
    match model.GraphIndex with
    | 0uy -> printf "｜ %f%% [%i/%i]" (getFloatValue model.Current model.Max) model.Current model.Max
    | 1uy -> printf "／ %f%% [%i/%i]" (getFloatValue model.Current model.Max) model.Current model.Max
    | 2uy -> printf "一 %f%% [%i/%i]" (getFloatValue model.Current model.Max) model.Current model.Max
    | 3uy -> printf "＼ %f%% [%i/%i]" (getFloatValue model.Current model.Max) model.Current model.Max
    | _ -> printf ""

let mutable private _current = Unchecked.defaultof<ProgressBarData>

let update i = 
    _current <- { _current with Current = i }
    drawModel _current

let private init max = 
    printf ""
    Console.CursorVisible <- false
    _current <- {
        Max = max
        Current = 0u
        GraphIndex = 0uy
    }

let show max = 
    init max
    while _current.Current <> _current.Max do
        drawModel _current
        async {
            do! Async.Sleep 100
        } |> Async.RunSynchronously
        _current <- { _current with GraphIndex = if _current.GraphIndex > 2uy then 0uy else _current.GraphIndex + 1uy }
    
    _current <- Unchecked.defaultof<ProgressBarData>
    Console.CursorLeft <- 0
    printf " 　　　　　　　　　　　　　　　　　　　　　　　　　　　　　"
    Console.CursorLeft <- 0
    Console.CursorVisible <- true