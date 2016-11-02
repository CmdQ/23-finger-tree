module CmdQ.Monoid

/// Helper type to make any default-constructable type a singleton.
type Singleton<'a when 'a : (new : unit -> 'a)> private() =
    static let instance = new 'a()
    static member Instance = instance

/// A type that provides a zero element and an associative add method.
type IMonoid<'m> =
    abstract member Zero:'m
    abstract member Add:'m -> 'm

/// A type that provides access to its measure.
type IMeasured<'m, 'a when 'm :> IMonoid<'m>> =
    abstract member Measure:'m

/// Return the measure of a measured type.
let fmeasure m = (m :> IMeasured<_, _>).Measure

/// Associatively combine the measures of all elements in a list.
let mconcat monoids = monoids |> (List.map fmeasure >> List.reduce (fun a b -> a.Add b))
