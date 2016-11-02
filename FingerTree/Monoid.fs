module CmdQ.Monoid

type Singleton<'a when 'a : (new : unit -> 'a)> private() =
    static let instance = new 'a()
    static member Instance = instance

type IMonoid<'m> =
    abstract member Zero:'m
    abstract member Add:'m -> 'm

type IMeasured<'m, 'a when 'm :> IMonoid<'m>> =
    abstract member Measure:'m

let fmeasure m = (m :> IMeasured<_, _>).Measure

let mconcat monoids = monoids |> (List.map fmeasure >> List.reduce (fun a b -> a.Add b))
