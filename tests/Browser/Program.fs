namespace Browser.Tests

open System
open System.Collections.Generic
open System.IO
open Expecto
open Microsoft.Playwright

module Program =

    type private Options = {
        ServerDll: string
        ArtifactsDirectory: string
        Headed: bool
        ExpectoArguments: string array
    }

    let rec private findRepositoryRoot directory =
        if File.Exists(Path.Combine(directory, "Application.sln")) then
            directory
        else
            match Directory.GetParent directory with
            | null -> failwith "Could not locate the paket-lock-diff repository root."
            | parent -> findRepositoryRoot parent.FullName

    let private parseOptions (args: string array) =
        let repositoryRoot =
            Directory.GetCurrentDirectory()
            |> Path.GetFullPath
            |> findRepositoryRoot

        let mutable serverDll =
            Path.Combine(repositoryRoot, ".artifacts", "e2e", "server", "Server.dll")

        let mutable artifactsDirectory =
            Path.Combine(repositoryRoot, ".artifacts", "e2e", "results")

        let mutable headed = false
        let expectoArguments = ResizeArray<string>()
        let mutable index = 0

        while index < args.Length do
            match args[index] with
            | "--server-dll" when index + 1 < args.Length ->
                serverDll <- args[index + 1]
                index <- index + 2
            | "--artifacts" when index + 1 < args.Length ->
                artifactsDirectory <- args[index + 1]
                index <- index + 2
            | "--headed" ->
                headed <- true
                index <- index + 1
            | argument ->
                expectoArguments.Add argument
                index <- index + 1

        {
            ServerDll = Path.GetFullPath serverDll
            ArtifactsDirectory = Path.GetFullPath artifactsDirectory
            Headed = headed
            ExpectoArguments = expectoArguments.ToArray()
        }

    let private runSuite (options: Options) =
        Directory.CreateDirectory options.ArtifactsDirectory
        |> ignore

        use server = RunningServer.Start options.ServerDll

        use playwright = Playwright.CreateAsync().GetAwaiter().GetResult()

        let browser =
            playwright.Chromium
                .LaunchAsync(BrowserTypeLaunchOptions(Headless = not options.Headed))
                .GetAwaiter()
                .GetResult()

        try
            let configuration = {
                Browser = browser
                BaseUrl = server.BaseUrl
                ArtifactsDirectory = options.ArtifactsDirectory
                ServerLogs = fun () -> server.Logs
            }

            AppTests.all configuration
            |> runTestsWithCLIArgs [] options.ExpectoArguments
        finally
            browser.CloseAsync().GetAwaiter().GetResult()

    [<EntryPoint>]
    let main args =
        match args with
        | [| "install"; browser |] ->
            Microsoft.Playwright.Program.Main(
                [|
                    "install"
                    browser
                |]
            )
        | [| "install"; "--with-deps"; browser |] ->
            Microsoft.Playwright.Program.Main(
                [|
                    "install"
                    "--with-deps"
                    browser
                |]
            )
        | _ ->
            args
            |> parseOptions
            |> runSuite
