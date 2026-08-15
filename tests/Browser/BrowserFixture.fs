namespace Browser.Tests

open System
open System.Collections.Generic
open System.IO
open System.Text.RegularExpressions
open Expecto
open Microsoft.Playwright

type BrowserConfiguration = {
    Browser: IBrowser
    BaseUrl: string
    ArtifactsDirectory: string
    ServerLogs: unit -> string
}

module BrowserFixture =

    let private slug (value: string) =
        Regex.Replace(value.ToLowerInvariant(), "[^a-z0-9]+", "-").Trim '-'

    let private awaitTask (task: System.Threading.Tasks.Task) =
        task
        |> Async.AwaitTask

    let private captureFailure
        configuration
        name
        (context: IBrowserContext)
        (page: IPage)
        (browserErrors: ResizeArray<string>)
        =
        async {
            let directory = Path.Combine(configuration.ArtifactsDirectory, slug name)

            Directory.CreateDirectory directory
            |> ignore

            File.WriteAllText(Path.Combine(directory, "server.log"), configuration.ServerLogs())

            if browserErrors.Count > 0 then
                File.WriteAllLines(Path.Combine(directory, "browser-errors.log"), browserErrors)

            try
                do!
                    page.ScreenshotAsync(
                        PageScreenshotOptions(
                            Path = Path.Combine(directory, "failure.png"),
                            FullPage = true
                        )
                    )
                    |> awaitTask
            with _ ->
                ()

            try
                do!
                    context.Tracing.StopAsync(
                        TracingStopOptions(Path = Path.Combine(directory, "trace.zip"))
                    )
                    |> awaitTask
            with _ ->
                ()
        }

    let private installOfflineRoutes (context: IBrowserContext) = async {
        do!
            context.RouteAsync(
                "https://cdn.jsdelivr.net/**",
                fun route ->
                    route.FulfillAsync(
                        RouteFulfillOptions(Status = 200, ContentType = "text/css", Body = "")
                    )
            )
            |> awaitTask

        do!
            context.RouteAsync(
                "https://raw.githubusercontent.com/fsprojects/Paket/master/docs/files/img/logo.png",
                fun route ->
                    route.FulfillAsync(
                        RouteFulfillOptions(
                            Status = 200,
                            ContentType = "image/png",
                            BodyBytes = Array.empty
                        )
                    )
            )
            |> awaitTask
    }

    let private createTestCase
        configuration
        name
        isAllowedBrowserError
        (body: IPage -> Async<unit>)
        =
        testCaseAsync name
        <| async {
            let! context =
                configuration.Browser.NewContextAsync(
                    BrowserNewContextOptions(
                        BaseURL = configuration.BaseUrl,
                        Permissions = [|
                            "clipboard-read"
                            "clipboard-write"
                        |],
                        ServiceWorkers = ServiceWorkerPolicy.Block,
                        ViewportSize = ViewportSize(Width = 1280, Height = 900)
                    )
                )
                |> Async.AwaitTask

            let browserErrors = ResizeArray<string>()

            do!
                context.Tracing.StartAsync(
                    TracingStartOptions(Screenshots = true, Snapshots = true, Sources = true)
                )
                |> awaitTask

            do! installOfflineRoutes context

            let! page =
                context.NewPageAsync()
                |> Async.AwaitTask

            page.PageError.Add(fun error -> browserErrors.Add $"Page error: {error}")

            page.Console.Add(fun message ->
                if message.Type = "error" then
                    browserErrors.Add $"Console error: {message.Text}"
            )

            let! outcome = async {
                try
                    do! body page

                    let unexpectedBrowserErrors =
                        browserErrors
                        |> Seq.filter (
                            isAllowedBrowserError
                            >> not
                        )
                        |> Seq.toArray

                    if unexpectedBrowserErrors.Length > 0 then
                        failtestf
                            "The browser reported errors: %s"
                            (String.concat " | " unexpectedBrowserErrors)

                    return Ok()
                with error ->
                    return Error error
            }

            match outcome with
            | Ok() ->
                do!
                    context.Tracing.StopAsync()
                    |> awaitTask

                do!
                    context.CloseAsync()
                    |> awaitTask
            | Error error ->
                do! captureFailure configuration name context page browserErrors

                do!
                    context.CloseAsync()
                    |> awaitTask

                return raise error
        }

    let testCase configuration name body =
        createTestCase configuration name (fun _ -> false) body

    let testCaseAllowingBrowserError configuration name isAllowedBrowserError body =
        createTestCase configuration name isAllowedBrowserError body

    let fulfill (status: int) contentType body (route: IRoute) =
        let headers = Dictionary<string, string>()

        headers["Access-Control-Allow-Origin"] <- "*"

        route.FulfillAsync(
            RouteFulfillOptions(
                Status = Nullable status,
                ContentType = contentType,
                Body = body,
                Headers = headers
            )
        )

    let fulfillText body route = fulfill 200 "text/plain" body route

    let fulfillJson body route =
        fulfill 200 "application/json" body route

    let gotoReady (page: IPage) relativeUrl = async {
        let! _ =
            page.GotoAsync(
                relativeUrl,
                PageGotoOptions(WaitUntil = WaitUntilState.DOMContentLoaded)
            )
            |> Async.AwaitTask

        do!
            page.Locator("html[data-paket-lock-client-ready=\"true\"]").WaitForAsync()
            |> awaitTask

        let! _ =
            page.WaitForFunctionAsync("() => typeof window.htmx !== 'undefined'")
            |> Async.AwaitTask

        return ()
    }

    let waitForResults (page: IPage) =
        page.Locator("#comparison-results [data-output-root]").WaitForAsync()
        |> awaitTask
