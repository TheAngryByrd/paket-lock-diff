namespace Shared

open System

type PaketLocks = {
    OlderLockFile : string
    NewerLockFile : string
}

module PaketLocks =
    let create (olderLockFile : string) (newerLockFile : string) =
        {
            OlderLockFile = olderLockFile
            NewerLockFile = newerLockFile
        }

type Package = {
    GroupName : string
    PackageName : string
    Version : string
}
type PackageVersionDiff = {
    GroupName : string
    PackageName : string
    OlderVersion : string
    NewerVersion : string
}

type PaketDiff = {
    Additions : Package list
    Removals : Package list
    VersionUpgrades : PackageVersionDiff list
    VersionDowngrades : PackageVersionDiff list
}

module Route =
    let builder typeName methodName = sprintf "/api/%s/%s" typeName methodName

type ITodosApi =
    {
        comparePaketLocks: PaketLocks -> Async<PaketDiff>
    }
