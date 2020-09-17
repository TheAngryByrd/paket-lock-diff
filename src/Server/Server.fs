module Server

open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Saturn

open Shared

module PaketComparer =
    open Paket
    open Paket.Domain
    type Package = {
        GroupName : GroupName
        PackageName : PackageName
        Version : SemVerInfo
    }
        with
            static member OfTuple (groupName, packageName, version) =
                {
                    GroupName = groupName
                    PackageName = packageName
                    Version = version
                }
    type PackageVersionDiff = {
        GroupName : GroupName
        PackageName : PackageName
        PreviousVersion : SemVerInfo
        PostVersion : SemVerInfo
    }

    type Diff = {
        Additions : Package list
        Removals : Package list
        VersionIncreases : PackageVersionDiff list
        VersionDecreases : PackageVersionDiff list
    }
    let compare (older, newer) =
        let olderPaketLock = Paket.LockFile.LoadFrom older
        let olderSet = olderPaketLock.InstalledPackages |> Set
        let newerPaketLock = Paket.LockFile.LoadFrom newer
        let newerSet = newerPaketLock.InstalledPackages |> Set
        let findPackageByGroupAndName packages (groupName, packageName) =
            packages
            |> List.tryFind(fun (g1,p1,_) -> groupName = g1 && packageName = p1)
        let additions =
            // Need sets without version numbers
            let os1 = olderSet |> Set.map(fun (g,p,v) -> g,p)
            let ns1 = newerSet |> Set.map(fun (g,p,v) -> g,p)
            Set.difference ns1 os1
            |> Set.toList
            |> List.choose(findPackageByGroupAndName newerPaketLock.InstalledPackages)
            |> List.map Package.OfTuple
        let removals =
            // Need sets without version numbers
            let os1 = olderSet |> Set.map(fun (g,p,v) -> g,p)
            let ns1 = newerSet |> Set.map(fun (g,p,v) -> g,p)
            Set.difference os1 ns1
            |> Set.toList
            |> List.choose(findPackageByGroupAndName olderPaketLock.InstalledPackages)
            |> List.map Package.OfTuple
        let versionIncreaes = []
        let versionDecreases = []
        {
            Additions = additions
            Removals = removals
            VersionIncreases = versionIncreaes
            VersionDecreases = versionDecreases
        }

type Storage () =
    let todos = ResizeArray<_>()

    member __.GetTodos () =
        List.ofSeq todos

    member __.AddTodo (todo: Todo) =
        if Todo.isValid todo.Description then
            todos.Add todo
            Ok ()
        else Error "Invalid todo"

let storage = Storage()

storage.AddTodo(Todo.create "Create new SAFE project") |> ignore
storage.AddTodo(Todo.create "Write your app") |> ignore
storage.AddTodo(Todo.create "Ship it !!!") |> ignore

let todosApi =
    { getTodos = fun () -> async { return storage.GetTodos() }
      addTodo =
        fun todo -> async {
            match storage.AddTodo todo with
            | Ok () -> return todo
            | Error e -> return failwith e
        } }

let webApp =
    Remoting.createApi()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.fromValue todosApi
    |> Remoting.buildHttpHandler

let app =
    application {
        url "http://0.0.0.0:8085"
        use_router webApp
        memory_cache
        use_static "public"
        use_gzip
    }

run app
