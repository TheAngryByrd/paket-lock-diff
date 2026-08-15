open Fake.Core
open Fake.IO
open Farmer
open Farmer.Builders

open Helpers

initializeContext ()

let sharedPath = Path.getFullName "src/Shared"
let serverPath = Path.getFullName "src/Server"
let clientPath = Path.getFullName "src/Client"
let clientOutputPath = Path.getFullName "src/Server/wwwroot/output"
let deployPath = Path.getFullName "deploy"
let sharedTestsPath = Path.getFullName "tests/Shared"
let serverTestsPath = Path.getFullName "tests/Server"
let clientTestsPath = Path.getFullName "tests/Client"

let release = ReleaseNotes.load "RELEASE_NOTES.md"

let buildVersion = [
    $"/p:Version={release.NugetVersion}"
    $"/p:AssemblyVersion={release.NugetVersion}"
]

Target.create
    "Clean"
    (fun _ ->
        Shell.cleanDir deployPath
        Shell.cleanDir clientOutputPath

        run
            dotnet
            [
                "fable"
                "clean"
                "--yes"
            ]
            clientPath
    )

Target.create "RestoreClientDependencies" (fun _ -> run npm [ "ci" ] ".")

Target.create
    "Bundle"
    (fun _ ->
        run
            dotnet
            [
                "fable"
                "-o"
                clientOutputPath
            ]
            clientPath

        run
            dotnet
            [
                "publish"
                "-c"
                "Release"
                "-o"
                deployPath
                yield! buildVersion
            ]
            serverPath
    )

Target.create
    "Azure"
    (fun _ ->
        let web = webApp {
            name "paket-lock-diff"
            operating_system OS.Windows
            runtime_stack (DotNet "10.0")
            zip_deploy "deploy"
        }

        let deployment = arm {
            location Location.WestEurope
            add_resource web
        }

        deployment
        |> Deploy.execute "paket_lock_diff_rg" Deploy.NoParameters
        |> ignore
    )

Target.create
    "Run"
    (fun _ ->
        run
            dotnet
            [
                "restore"
                "Application.sln"
            ]
            "."

        run dotnet [ "build" ] sharedPath

        run
            dotnet
            [
                "fable"
                "-o"
                clientOutputPath
            ]
            clientPath

        [
            "server",
            dotnet
                [
                    "watch"
                    "run"
                    "--no-restore"
                    yield! buildVersion
                ]
                serverPath
            "client",
            dotnet
                [
                    "fable"
                    "watch"
                    "-o"
                    clientOutputPath
                ]
                clientPath
        ]
        |> runParallel
    )

let buildSharedTests () = run dotnet [ "build" ] sharedTestsPath

Target.create
    "RunTestsHeadless"
    (fun _ ->
        buildSharedTests ()

        run dotnet [ "run" ] serverTestsPath

        run
            dotnet
            [
                "fable"
                "-o"
                "output"
            ]
            clientTestsPath

        run
            npx
            [
                "mocha"
                "output/Client.Tests.js"
            ]
            clientTestsPath
    )

Target.create
    "WatchRunTests"
    (fun _ ->
        buildSharedTests ()

        [
            "server",
            dotnet
                [
                    "watch"
                    "run"
                ]
                serverTestsPath
            "client",
            dotnet
                [
                    "fable"
                    "watch"
                    "-o"
                    "output"
                    "--verbose"
                    "--run"
                    "npx"
                    "mocha"
                    "--watch"
                    "output/Client.Tests.js"
                ]
                clientTestsPath
        ]
        |> runParallel
    )

Target.create
    "Format"
    (fun _ ->
        run
            dotnet
            [
                "fantomas"
                "."
            ]
            "."
    )

open Fake.Core.TargetOperators

let dependencies = [
    "Clean"
    ==> "Bundle"
    ==> "Azure"

    "Clean"
    ==> "Run"

    "RestoreClientDependencies"
    ==> "RunTestsHeadless"
    "RestoreClientDependencies"
    ==> "WatchRunTests"
]

[<EntryPoint>]
let main args = runOrDefault args
