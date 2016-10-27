module CmdQ.Monoid

type Singleton<'a when 'a : (new : unit -> 'a)> private() =
    static let instance = new 'a()
    static member Instance = instance

type IMonoid<'m> =
    abstract member Zero:'m
    abstract member Add:'m -> 'm
    abstract member Add2:'m -> 'm -> 'm

type IMeasured<'m, 'a when 'm :> IMonoid<'m>> =
    abstract member Measure:'m

let fmeasure m = (m :> IMeasured<_, _>).Measure

let mconcat = function
    | [] -> invalidArg "" "Need at least one element."
    | [x] -> fmeasure x
    | x::xs ->
        let monoid = fmeasure x
        xs |> List.map fmeasure |> List.fold monoid.Add2 monoid
