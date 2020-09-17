module Server.Tests

open Expecto

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
    testCaseAsync "Additions" <| async {
        let older = "./paket-lock-files/addition-tests/old-paket.lock"
        let newer = "./paket-lock-files/addition-tests/new-paket.lock"
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