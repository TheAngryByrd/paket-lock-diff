namespace Server.Core


type Async =
    static member map f x =
        async.Bind(x, (fun v -> async.Return(f v)))

    /// <summary>
    /// Executes two asyncs concurrently and returns a tuple of the values
    /// </summary>
    /// <param name="a1">An async to execute</param>
    /// <param name="a2">An async to execute</param>
    /// <returns>Tuple of computed values</returns>
    static member parZip (a1: Async<'a>) (a2: Async<'b>) =
        // it is not advised to use async {} blocks in the implementation because it can go recursive... see https://thinkbeforecoding.com/post/2020/10/07/applicative-computation-expressions
        // This is the same as:
        // async {
        //     let! c1 = a1 |> Async.StartChild
        //     let! c2 = a2 |> Async.StartChild
        //     let! r1 = c1
        //     let! r2 = c2
        //     return r1,r2
        // }
        async.Bind(
            Async.StartChild a1,
            fun c1 ->
                async.Bind(
                    Async.StartChild a2,
                    fun c2 ->
                        async.Bind(c1, (fun r1 -> async.Bind(c2, (fun r2 -> async.Return(r1, r2)))))
                )
        )

type ParallelAsyncBuilder() =
    member __.Zero() = async.Zero()

    member __.Delay generator = async.Delay generator

    member inline __.Return value = async.Return value

    member inline __.ReturnFrom(computation: Async<_>) = async.ReturnFrom computation

    member inline __.Bind(computation, binder) = async.Bind(computation, binder)

    member __.Using(resource, binder) = async.Using(resource, binder)

    member __.While(guard, computation) = async.While(guard, computation)

    member __.For(sequence, body) = async.For(sequence, body)

    member inline __.Combine(computation1, computation2) =
        async.Combine(computation1, computation2)

    member inline __.TryFinally(computation, compensation) =
        async.TryFinally(computation, compensation)

    member inline __.TryWith(computation, catchHandler) =
        async.TryWith(computation, catchHandler)

    member inline __.BindReturn(x: Async<'T>, f) = Async.map f x

    member inline __.MergeSources(t1: Async<'T>, t2: Async<'T1>) = Async.parZip t1 t2

[<AutoOpen>]
module Asyncs =
    /// <summary>
    /// Async computation expression which allows for parallel execution of asyncs with the applicative (and!) syntax
    /// </summary>
    /// <returns></returns>
    let parAsync = ParallelAsyncBuilder()