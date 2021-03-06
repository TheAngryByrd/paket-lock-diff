module Server

open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Saturn
open Shared
open System
open Microsoft.AspNetCore.Http

module String =
    open System
    let splitByNewlines (s : string) =
        // s.Split([|'\n'; '\r'|]) Using an array seems to trim the output also. WHY?
        s.Split('\n') |> Array.collect(fun x -> x.Split('\r'))

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
    type SemVerChange =
    | Major
    | Minor
    | Patch
    | Other

    type PackageVersionDiff = {
        GroupName : GroupName
        PackageName : PackageName
        OlderVersion : SemVerInfo
        NewerVersion : SemVerInfo
        SemVerChange : SemVerChange
    }

    type Diff = {
        Additions : Package list
        Removals : Package list
        VersionUpgrades : PackageVersionDiff list
        VersionDowngrades : PackageVersionDiff list
    }

    let diffToDTO ( d : Diff) =
        let toPackageDTO (p : Package) =
            {
                Shared.Package.GroupName = p.GroupName.Name
                PackageName = p.PackageName.Name
                Version = p.Version.Normalize()
            }
        let toPackageVersionDiffDTO (p : PackageVersionDiff) =
            {
                Shared.PackageVersionDiff.GroupName = p.GroupName.Name
                PackageName = p.PackageName.Name
                OlderVersion = p.OlderVersion.Normalize()
                NewerVersion = p.NewerVersion.Normalize()
                SemVerChange =
                    match p.SemVerChange with
                    | Major -> Shared.SemVerChange.Major
                    | Minor -> Shared.SemVerChange.Minor
                    | Patch -> Shared.SemVerChange.Patch
                    | Other -> Shared.SemVerChange.Other
            }
        {
            PaketDiff.Additions = d.Additions |> List.map toPackageDTO
            Removals = d.Removals |> List.map toPackageDTO
            VersionUpgrades = d.VersionUpgrades |> List.map toPackageVersionDiffDTO
            VersionDowngrades = d.VersionDowngrades |> List.map toPackageVersionDiffDTO
        }

    let compare (older, newer) =
        let olderPaketLock = Paket.LockFile.Parse("old.lock", older)

        // Need sets without version numbers
        let olderSet =
            olderPaketLock.InstalledPackages
            |> Set
            |> Set.map(fun (g,p,v) -> g,p)
        let newerPaketLock = Paket.LockFile.Parse("new.lock", newer)
        let newerSet =
            newerPaketLock.InstalledPackages
            |> Set
            |> Set.map(fun (g,p,v) -> g,p)
        let findPackageByGroupAndName packages (groupName, packageName) =
            packages
            |> List.tryFind(fun (g1,p1,_) -> groupName = g1 && packageName = p1)

        let additions =
            Set.difference newerSet olderSet
            |> Set.toList
            |> List.choose(findPackageByGroupAndName newerPaketLock.InstalledPackages)
            |> List.map Package.OfTuple
        let removals =
            // Need sets without version numbers
            Set.difference olderSet newerSet
            |> Set.toList
            |> List.choose(findPackageByGroupAndName olderPaketLock.InstalledPackages)
            |> List.map Package.OfTuple

        let calculateSemVerChange (olderVersion : SemVerInfo) (newerVersion : SemVerInfo) =
            if olderVersion.Major <> newerVersion.Major then
                SemVerChange.Major
            elif olderVersion.Minor <> newerVersion.Minor then
                SemVerChange.Minor
            elif olderVersion.Patch <> newerVersion.Patch then
                SemVerChange.Patch
            else
                SemVerChange.Other

        let packagesChanged =
            Set.intersect olderSet newerSet
            |> Set.toList
            |> List.choose(fun (g,p) ->
                let older = findPackageByGroupAndName olderPaketLock.InstalledPackages (g,p)
                let newer = findPackageByGroupAndName newerPaketLock.InstalledPackages (g,p)
                match older, newer with
                | Some (_,_,olderVersion), Some (_,_,newerVersion)  ->
                    Some {
                        GroupName = g
                        PackageName = p
                        OlderVersion = olderVersion
                        NewerVersion = newerVersion
                        SemVerChange = calculateSemVerChange olderVersion newerVersion
                    }
                | _ -> None
            )

        let versionIncreaes =
            packagesChanged
            |> List.filter(fun p -> p.OlderVersion < p.NewerVersion)

        let versionDowngrades =
            packagesChanged
            |> List.filter(fun p -> p.OlderVersion > p.NewerVersion)

        {
            Additions = additions
            Removals = removals
            VersionUpgrades = versionIncreaes
            VersionDowngrades = versionDowngrades
        }

let todosApi =
    {
        comparePaketLocks = fun paketFiles -> async {
            let olderSplit = paketFiles.OlderLockFile |> String.splitByNewlines
            let newerSplit = paketFiles.NewerLockFile |> String.splitByNewlines
            let comparison = PaketComparer.compare(olderSplit, newerSplit)
            return comparison |> PaketComparer.diffToDTO
        }
    }

let errorHandler (ex : Exception) (routeInfo: RouteInfo<HttpContext>) =
    printfn "%A" ex
    match ex with
    | ex when ex.Message.Contains("Error during parsing") ->
        let error =
            {
                Message = ex.Message
                InnerMessage = ex.InnerException.Message
                StackTrace = ex.StackTrace
            }
        Propagate error
    | ex ->
        Propagate ex.Message

let webApp =
    Remoting.createApi()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.withErrorHandler errorHandler
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
