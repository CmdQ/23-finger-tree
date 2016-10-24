module CmdQ.Tests.Benchmark

open CmdQ
open Fuchu
open Fuchu.FuchuPerfUtil
open PerfUtil
open System

type ISubject =
    inherit ITestable

    abstract member InsertOrAppend : int -> unit

[<AbstractClass>]
type NamedTest(name) =
    interface ITestable with
        member __.Init () = ()
        member __.Fini () = ()
        member __.Name = name

type Subject() =
    inherit NamedTest("FingerTree")

    let mutable tree = Finger.empty

    interface ISubject with
        member __.InsertOrAppend what =
            tree <- tree |> (if what % 2 = 0 then Finger.append else Finger.prepend) what

type WithList() =
    inherit NamedTest("Linked List")

    let mutable list = []

    interface ISubject with
        member __.InsertOrAppend what =
            if what % 2 = 0 then
                list <- list @ [what]
            else
                list <- what::list

type WithResizeArray() =
    inherit NamedTest("ResizeArray")

    let ra = ResizeArray()

    interface ISubject with
        member __.InsertOrAppend what =
            if what % 2 = 0 then
                ra.Add what
            else
                ra.Insert(0, what)

let alts:ISubject list = [WithList(); WithResizeArray()]
let subj = Subject() :> ISubject

let plays : PerfTest<ISubject> list = [
    perfTest "Insert at beginning or end" (fun s ->
        let rand = Random(23)
        for i = 1 to 4000 do
            rand.Next(i) |> s.InsertOrAppend) 10
]

let benchmarks =
    testList "Benchmarks" [
        testPerfImpls "Finger Tree" subj alts plays
    ]
