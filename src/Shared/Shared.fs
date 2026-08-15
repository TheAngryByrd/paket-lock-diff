namespace Shared

open System


module String =
    let notNullOrEmpty (s: string) =
        String.IsNullOrWhiteSpace s
        |> not

type PaketLocks = {
    OlderLockFile: string
    NewerLockFile: string
}

module PaketLocks =
    let create (olderLockFile: string) (newerLockFile: string) = {
        OlderLockFile = olderLockFile
        NewerLockFile = newerLockFile
    }

type Package = {
    GroupName: string
    PackageName: string
    Version: string
}

type SemVerChange =
    | Major
    | Minor
    | Patch
    | Other

type PackageVersionDiff = {
    GroupName: string
    PackageName: string
    OlderVersion: string
    NewerVersion: string
    SemVerChange: SemVerChange
}

type PaketDiff = {
    Additions: Package list
    Removals: Package list
    VersionUpgrades: PackageVersionDiff list
    VersionDowngrades: PackageVersionDiff list
}

type VersionInfo = {
    PaketLockDiff: string
    PaketCore: string
}
