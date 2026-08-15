namespace Server

module Handlers =

    open System
    open System.Reflection
    open Giraffe
    open Giraffe.Htmx
    open Giraffe.ViewEngine
    open Microsoft.AspNetCore.Http
    open Microsoft.Extensions.DependencyInjection
    open Microsoft.Extensions.Logging
    open Shared
    open Server.Core

    let private queryValue name (ctx: HttpContext) =
        match ctx.Request.Query.TryGetValue name with
        | true, values -> string values
        | false, _ -> ""

    let private versionInfo () =
        let assembly = Assembly.GetExecutingAssembly()

        let paketCoreVersion =
            assembly.GetReferencedAssemblies()
            |> Array.tryFind (fun reference -> reference.Name = "Paket.Core")
            |> Option.bind (fun reference -> Option.ofObj reference.Version)
            |> Option.map string
            |> Option.defaultValue "unknown"

        {
            PaketCore = paketCoreVersion
            PaketLockDiff =
                assembly.GetName().Version
                |> string
        }

    let private htmlFragment view =
        view
        |> RenderView.AsString.htmlNode
        |> htmlString

    let index: HttpHandler =
        fun next ctx ->
            let inputType =
                queryValue "input" ctx
                |> Views.InputType.parse

            let olderUrl = queryValue "olderLockFileUrl" ctx
            let newerUrl = queryValue "newerLockFileUrl" ctx
            let githubUrl = queryValue "githubPullRequestUrl" ctx

            if ctx.Request.IsHtmx then
                Views.inputSection inputType olderUrl newerUrl githubUrl
                |> htmlFragment
                |> fun handler -> handler next ctx
            else
                Views.page inputType olderUrl newerUrl githubUrl (versionInfo ())
                |> htmlView
                |> fun handler -> handler next ctx

    let compare: HttpHandler =
        fun next ctx -> task {
            let! form = ctx.Request.ReadFormAsync(ctx.RequestAborted)
            let olderLockFile = string form["olderLockFile"]
            let newerLockFile = string form["newerLockFile"]

            if
                String.IsNullOrWhiteSpace olderLockFile
                || String.IsNullOrWhiteSpace newerLockFile
            then
                return!
                    Views.validationError
                        "Provide an older and a newer paket.lock file, then try again."
                    |> htmlFragment
                    |> fun handler -> handler next ctx
            else
                try
                    let olderLines =
                        olderLockFile
                        |> String.splitByNewlines

                    let newerLines =
                        newerLockFile
                        |> String.splitByNewlines

                    let! comparison =
                        Async.StartAsTask(
                            PaketComparer.compare (olderLines, newerLines),
                            cancellationToken = ctx.RequestAborted
                        )

                    let diff =
                        comparison
                        |> PaketComparer.diffToDTO

                    let reportUrl =
                        ctx.Request.Headers.HxCurrentUrl
                        |> Option.map string
                        |> Option.defaultValue "/"

                    return!
                        Views.results reportUrl diff
                        |> htmlFragment
                        |> fun handler -> handler next ctx
                with
                | :? OperationCanceledException when ctx.RequestAborted.IsCancellationRequested ->
                    return None
                | error ->
                    let logger =
                        ctx.RequestServices
                            .GetRequiredService<ILoggerFactory>()
                            .CreateLogger("Server.Handlers")

                    logger.LogWarning(
                        "Lock-file comparison failed with {ExceptionType}; request {TraceIdentifier}",
                        error.GetType().FullName,
                        ctx.TraceIdentifier
                    )

                    return!
                        Views.comparisonError error
                        |> htmlFragment
                        |> fun handler -> handler next ctx
        }
