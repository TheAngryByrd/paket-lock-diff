namespace Server.Core


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
