module Server


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

module String =
    let splitByNewlines (s: string) =
        // s.Split([|'\n'; '\r'|]) Using an array seems to trim the output also. WHY?
        s.Split('\n')
        |> Array.collect (fun x -> x.Split('\r'))

module PaketComparer =
    open Paket
    open Paket.Domain
    open Shared

    type Package =
        {
            GroupName: GroupName
            PackageName: PackageName
            Version: SemVerInfo
        }

        static member OfTuple(groupName, packageName, version) = {
            GroupName = groupName
            PackageName = packageName
            Version = version
        }

    [<RequireQualifiedAccess>]
    type SemVerChange =
        | Major
        | Minor
        | Patch
        | Other

    type PackageVersionDiff = {
        GroupName: GroupName
        PackageName: PackageName
        OlderVersion: SemVerInfo
        NewerVersion: SemVerInfo
        SemVerChange: SemVerChange
    }

    type Diff = {
        Additions: Package array
        Removals: Package array
        VersionUpgrades: PackageVersionDiff array
        VersionDowngrades: PackageVersionDiff array
    }

    let diffToDTO (d: Diff) =
        let toPackageDTO (p: Package) = {
            Shared.Package.GroupName = p.GroupName.Name
            PackageName = p.PackageName.Name
            Version = p.Version.Normalize()
        }

        let toPackageVersionDiffDTO (p: PackageVersionDiff) = {
            Shared.PackageVersionDiff.GroupName = p.GroupName.Name
            PackageName = p.PackageName.Name
            OlderVersion = p.OlderVersion.Normalize()
            NewerVersion = p.NewerVersion.Normalize()
            SemVerChange =
                match p.SemVerChange with
                | SemVerChange.Major -> Shared.SemVerChange.Major
                | SemVerChange.Minor -> Shared.SemVerChange.Minor
                | SemVerChange.Patch -> Shared.SemVerChange.Patch
                | SemVerChange.Other -> Shared.SemVerChange.Other
        }

        {
            PaketDiff.Additions =
                d.Additions
                |> Array.map toPackageDTO
                |> Array.toList
            Removals =
                d.Removals
                |> Array.map toPackageDTO
                |> Array.toList
            VersionUpgrades =
                d.VersionUpgrades
                |> Array.map toPackageVersionDiffDTO
                |> Array.toList
            VersionDowngrades =
                d.VersionDowngrades
                |> Array.map toPackageVersionDiffDTO
                |> Array.toList
        }


    let calculateSemVerChange (olderVersion: SemVerInfo) (newerVersion: SemVerInfo) =
        if
            olderVersion.Major
            <> newerVersion.Major
        then
            SemVerChange.Major
        elif
            olderVersion.Minor
            <> newerVersion.Minor
        then
            SemVerChange.Minor
        elif
            olderVersion.Patch
            <> newerVersion.Patch
        then
            SemVerChange.Patch
        else
            SemVerChange.Other

    let compare (older, newer) = parAsync {
        let! olderPaketLock = async { return LockFile.Parse("old.lock", older) }
        and! newerPaketLock = async { return LockFile.Parse("new.lock", newer) }

        let findPackageByGroupAndName packages (groupName, packageName) =
            packages
            |> List.tryFind (fun (g1, p1, _) ->
                groupName = g1
                && packageName = p1
            )

        // Need sets without version numbers
        let olderSet =
            olderPaketLock.InstalledPackages
            |> Set
            |> Set.map (fun (g, p, _) -> g, p)

        let newerSet =
            newerPaketLock.InstalledPackages
            |> Set
            |> Set.map (fun (g, p, _) -> g, p)


        let packagesChanged =
            Set.intersect olderSet newerSet
            |> Set.toArray
            |> Array.Parallel.choose (fun (g, p) ->
                let older = findPackageByGroupAndName olderPaketLock.InstalledPackages (g, p)
                let newer = findPackageByGroupAndName newerPaketLock.InstalledPackages (g, p)

                match older, newer with
                | Some (_, _, olderVersion), Some (_, _, newerVersion) ->
                    Some
                        {
                            GroupName = g
                            PackageName = p
                            OlderVersion = olderVersion
                            NewerVersion = newerVersion
                            SemVerChange = calculateSemVerChange olderVersion newerVersion
                        }
                | _ -> None
            )

        let! additions = async {
            return
                Set.difference newerSet olderSet
                |> Set.toArray
                |> Array.Parallel.choose (
                    findPackageByGroupAndName newerPaketLock.InstalledPackages
                )
                |> Array.map Package.OfTuple
        }

        and! removals = async {
            return
                Set.difference olderSet newerSet
                |> Set.toArray
                |> Array.Parallel.choose (
                    findPackageByGroupAndName olderPaketLock.InstalledPackages
                )
                |> Array.map Package.OfTuple
        }

        and! versionIncreaes = async {
            return
                packagesChanged
                |> Array.filter (fun p -> p.OlderVersion < p.NewerVersion)
        }

        and! versionDowngrades = async {
            return
                packagesChanged
                |> Array.filter (fun p -> p.OlderVersion > p.NewerVersion)
        }

        return {
            Additions = additions
            Removals = removals
            VersionUpgrades = versionIncreaes
            VersionDowngrades = versionDowngrades
        }
    }


open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection

open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Saturn
open Shared
open System

let api (httpContext: HttpContext) = {
    comparePaketLocks =
        fun paketFiles -> async {
            let olderSplit =
                paketFiles.OlderLockFile
                |> String.splitByNewlines

            let newerSplit =
                paketFiles.NewerLockFile
                |> String.splitByNewlines

            let! comparison = PaketComparer.compare (olderSplit, newerSplit)

            return
                comparison
                |> PaketComparer.diffToDTO
        }
    versionInfo = fun () -> async {
        let references = System.Reflection.Assembly.GetExecutingAssembly().GetReferencedAssemblies();
        let paketCore = references |> Array.find(fun a -> a.Name.Contains("Paket.Core"))

        let appVersion = System.Reflection.Assembly.GetExecutingAssembly().GetName().Version.ToString()
        return {
            PaketCore = paketCore.Version.ToString()
            PaketLockDiff = appVersion
        }
    }
}


let errorHandler (ex: Exception) (routeInfo: RouteInfo<HttpContext>) =
    printfn "%A" ex

    match ex with
    | ex when ex.Message.Contains("Error during parsing") ->
        let error = {
            Message = ex.Message
            InnerMessage = ex.InnerException.Message
            StackTrace = ex.StackTrace
        }

        Propagate error
    | ex -> Propagate ex.Message

let webApp =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.withErrorHandler errorHandler
    |> Remoting.fromContext api
    |> Remoting.buildHttpHandler

let configureServices (services: IServiceCollection) = services


let app = application {
    url "http://0.0.0.0:8085"
    use_router webApp
    memory_cache
    use_static "public"
    use_gzip
    service_config configureServices
}

[<EntryPoint>]
let main _ =
    run app
    0
