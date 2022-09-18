open Fake.Core
open Fake.IO
open Fake.Core
open Farmer
open Farmer.Builders

open Helpers

initializeContext()

let sharedPath = Path.getFullName "src/Shared"
let serverPath = Path.getFullName "src/Server"
let clientPath = Path.getFullName "src/Client"
let deployPath = Path.getFullName "deploy"
let sharedTestsPath = Path.getFullName "tests/Shared"
let serverTestsPath = Path.getFullName "tests/Server"
let clientTestsPath = Path.getFullName "tests/Client"

let release = ReleaseNotes.load "RELEASE_NOTES.md"

let buildVersion = $"/p:Version={release.NugetVersion} /p:AssemblyVersion={release.NugetVersion}"

Target.create "Clean" (fun _ ->
    Shell.cleanDir deployPath
    run dotnet "fable clean --yes" clientPath // Delete *.fs.js files created by Fable
)

Target.create "InstallClient" (fun _ -> run npm "install" ".")

Target.create "Bundle" (fun _ ->
    [ "server", dotnet $"publish -c Release -o \"{deployPath}\" {buildVersion}" serverPath
      "client", dotnet "fable -o output -s --run npm run build" clientPath ]
    |> runParallel
)

Target.create "Azure" (fun _ ->
    let web = webApp {
        name "paket-lock-diff"
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

Target.create "Run" (fun _ ->
    run dotnet "build" sharedPath
    [ "server", dotnet $"watch run -- {buildVersion}" serverPath
      "client", dotnet "fable watch -o output -s --run npm run start" clientPath ]
    |> runParallel
)

Target.create "RunTests" (fun _ ->
    run dotnet "build" sharedTestsPath
    [ "server", dotnet "watch run {buildVersion}" serverTestsPath
      "client", dotnet "fable watch -o output -s --run npm run test:live" clientTestsPath ]
    |> runParallel
)

Target.create "Format" (fun _ ->
    run dotnet "fantomas . -r" "src"
)

open Fake.Core.TargetOperators

let dependencies = [
    "Clean"
        ==> "InstallClient"
        ==> "Bundle"
        ==> "Azure"

    "Clean"
        ==> "InstallClient"
        ==> "Run"

    "InstallClient"
        ==> "RunTests"
]

[<EntryPoint>]
let main args = runOrDefault args
