module Server.Tests

open Expecto

open System
open Shared
open Server
open Paket
open Paket.Domain

let paketCompareTests = testList "Paket Compare" [
    let createPackage groupName packageName version =
        PaketComparer.Package.OfTuple(
            GroupName groupName,
            PackageName packageName,
            SemVer.Parse version
        )
    let createPackageVersionDiff groupName packageName olderVersion newerVersion =
        {
            PaketComparer.PackageVersionDiff.GroupName = GroupName groupName
            PaketComparer.PackageVersionDiff.PackageName = PackageName packageName
            PaketComparer.PackageVersionDiff.OlderVersion = SemVer.Parse olderVersion
            PaketComparer.PackageVersionDiff.NewerVersion = SemVer.Parse newerVersion
        }
    let readFile = IO.File.ReadAllText >> String.splitByNewlines
    testCaseAsync "Additions" <| async {
        let older = "./paket-lock-files/addition-tests/old-paket.lock" |> readFile
        let newer = "./paket-lock-files/addition-tests/new-paket.lock" |> readFile
        let result = PaketComparer.compare(older, newer)

        let expectedAdditions =
            [
                createPackage "main" "Chessie" "0.6.0"
                createPackage "main" "Microsoft.NETCore.Platforms" "3.1.3"
                createPackage "main" "Mono.Cecil" "0.11.3"
                createPackage "main" "NETStandard.Library" "2.0.3"
                createPackage "main" "Newtonsoft.Json" "12.0.3"
                createPackage "main" "Paket.Core" "5.249.2"
                createPackage "main" "System.Buffers" "4.5.1"
                createPackage "main" "System.Memory" "4.5.4"
                createPackage "main" "System.Net.Http.WinHttpHandler" "4.7.2"
                createPackage "main" "System.Numerics.Vectors" "4.5"
                createPackage "main" "System.Runtime.CompilerServices.Unsafe" "4.7.1"
                createPackage "main" "System.Security.Cryptography.ProtectedData" "4.7"
            ]
        Expect.sequenceEqual result.Additions expectedAdditions ""
        Expect.sequenceEqual result.Removals [] ""
    }
    testCaseAsync "Removals" <| async {
        let older = "./paket-lock-files/removal-tests/old-paket.lock" |> readFile
        let newer = "./paket-lock-files/removal-tests/new-paket.lock" |> readFile
        let result = PaketComparer.compare(older, newer)

        let expectedRemovals =
            [
                createPackage "main" "FsToolkit.ErrorHandling" "1.4.3"
            ]
        Expect.sequenceEqual result.Additions [] ""
        Expect.sequenceEqual result.Removals expectedRemovals ""
        Expect.sequenceEqual result.VersionIncreases [] ""
        Expect.sequenceEqual result.VersionDecreases [] ""
    }
    testCaseAsync "Version Increases" <| async {
        let older = "./paket-lock-files/version-increase-tests/old-paket.lock" |> readFile
        let newer = "./paket-lock-files/version-increase-tests/new-paket.lock" |> readFile
        let result = PaketComparer.compare(older, newer)

        let expectedIncreases =
            [
                createPackageVersionDiff "main" "FsToolkit.ErrorHandling" "1.4.0" "1.4.3"
            ]
        Expect.sequenceEqual result.Additions [] ""
        Expect.sequenceEqual result.Removals [] ""
        Expect.sequenceEqual result.VersionIncreases expectedIncreases ""
        Expect.sequenceEqual result.VersionDecreases [] ""
    }
    testCaseAsync "Version Decreases" <| async {
        let older = "./paket-lock-files/version-decrease-tests/old-paket.lock" |> readFile
        let newer = "./paket-lock-files/version-decrease-tests/new-paket.lock" |> readFile
        let result = PaketComparer.compare(older, newer)

        let expectedIncreases =
            [
                createPackageVersionDiff "main" "FsToolkit.ErrorHandling" "1.4.3" "1.4.0"
            ]
        Expect.sequenceEqual result.Additions [] ""
        Expect.sequenceEqual result.Removals [] ""
        Expect.sequenceEqual result.VersionIncreases [] ""
        Expect.sequenceEqual result.VersionDecreases expectedIncreases ""
    }
]

let server = testList "Server" [
    testCase "Adding valid Todo" <| fun _ ->
        let storage = Storage()
        let validTodo = Todo.create "TODO"
        let expectedResult = Ok ()

        let result = storage.AddTodo validTodo

        Expect.equal result expectedResult "Result should be ok"
        Expect.contains (storage.GetTodos()) validTodo "Storage should contain new todo"
]

let all =
    testList "All"
        [
            Shared.Tests.shared
            paketCompareTests
            server
        ]

[<EntryPoint>]
let main _ = runTests defaultConfig all