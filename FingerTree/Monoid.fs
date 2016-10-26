module CmdQ.Monoid

type Singleton<'a when 'a : (new : unit -> 'a)> private() =
    static let instance = new 'a()
    static member Instance = instance

type IMonoid<'a> =
    abstract member Zero:'a
    abstract member Add:'a -> 'a -> 'a

type IMeasured<'m, 'a when 'm :> IMonoid<'m>> =
    abstract member Measure:'m

let fmeasure m = (m :> IMeasured<_, _>).Measure

let mconcat = function
    | [] -> failwith ErrorMessages.patternMatchImpossible
    | [x] -> fmeasure x
    | x::xs ->
        let monoid = fmeasure x
        xs |> List.map fmeasure |> List.fold monoid.Add monoid
