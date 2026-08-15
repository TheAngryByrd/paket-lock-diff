namespace Browser.Tests

open System
open System.Collections.Concurrent
open System.Diagnostics
open System.IO
open System.Net
open System.Net.Http
open System.Net.Sockets
open System.Threading

type RunningServer
    private (baseUrl: string, childProcess: Process option, logs: ConcurrentQueue<string>) =

    member _.BaseUrl = baseUrl.TrimEnd '/'

    member _.Logs =
        logs
        |> Seq.toArray
        |> String.concat Environment.NewLine

    interface IDisposable with
        member _.Dispose() =
            childProcess
            |> Option.iter (fun child ->
                try
                    if not child.HasExited then
                        child.Kill(true)

                    child.WaitForExit(5_000)
                    |> ignore
                with _ ->
                    ()

                child.Dispose()
            )

    static member private FreePort() =
        use listener = new TcpListener(IPAddress.Loopback, 0)
        listener.Start()
        let endpoint = listener.LocalEndpoint :?> IPEndPoint
        endpoint.Port

    static member private WaitUntilReady
        (childProcess: Process, baseUrl: string, logs: ConcurrentQueue<string>)
        =
        use client = new HttpClient(Timeout = TimeSpan.FromSeconds 2.)
        let deadline = DateTime.UtcNow.AddSeconds 30.
        let mutable ready = false

        while not ready
              && DateTime.UtcNow < deadline do
            if childProcess.HasExited then
                failwith
                    $"The E2E server exited with code {childProcess.ExitCode} before it was ready.{Environment.NewLine}{String.concat Environment.NewLine logs}"

            try
                use response = client.GetAsync(baseUrl).GetAwaiter().GetResult()

                ready <- response.IsSuccessStatusCode
            with _ ->
                ()

            if not ready then
                Thread.Sleep 100

        if not ready then
            failwith
                $"The E2E server did not become ready at {baseUrl}.{Environment.NewLine}{String.concat Environment.NewLine logs}"

    static member Start(serverDll: string) =
        match Environment.GetEnvironmentVariable "E2E_BASE_URL" with
        | externalUrl when not (String.IsNullOrWhiteSpace externalUrl) ->
            new RunningServer(externalUrl, None, ConcurrentQueue<string>())
        | _ ->
            if not (File.Exists serverDll) then
                failwith
                    $"The published server was not found at {serverDll}. Run `dotnet run -- BuildBrowserTests` first."

            let configuredPort = Environment.GetEnvironmentVariable "PAKET_LOCK_DIFF_E2E_PORT"

            let port =
                match Int32.TryParse configuredPort with
                | true, value when value > 0 -> value
                | _ -> RunningServer.FreePort()

            let baseUrl = $"http://127.0.0.1:{port}"
            let logs = ConcurrentQueue<string>()
            let startInfo = ProcessStartInfo()
            startInfo.FileName <- "dotnet"
            startInfo.ArgumentList.Add(Path.GetFullPath serverDll)
            startInfo.ArgumentList.Add "--urls"
            startInfo.ArgumentList.Add baseUrl
            startInfo.WorkingDirectory <- Path.GetDirectoryName(Path.GetFullPath serverDll)
            startInfo.UseShellExecute <- false
            startInfo.RedirectStandardOutput <- true
            startInfo.RedirectStandardError <- true
            startInfo.Environment["ASPNETCORE_ENVIRONMENT"] <- "Production"

            let childProcess = new Process(StartInfo = startInfo)

            childProcess.OutputDataReceived.Add(fun event ->
                if not (isNull event.Data) then
                    logs.Enqueue event.Data
            )

            childProcess.ErrorDataReceived.Add(fun event ->
                if not (isNull event.Data) then
                    logs.Enqueue event.Data
            )

            if not (childProcess.Start()) then
                failwith "The E2E server process could not be started."

            childProcess.BeginOutputReadLine()
            childProcess.BeginErrorReadLine()

            try
                RunningServer.WaitUntilReady(childProcess, baseUrl, logs)
                new RunningServer(baseUrl, Some childProcess, logs)
            with error ->
                if not childProcess.HasExited then
                    childProcess.Kill(true)

                childProcess.Dispose()
                raise error
