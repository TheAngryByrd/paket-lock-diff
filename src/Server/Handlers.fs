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

    let private sanitizedExceptionForLogging (error: exn) =
        let exceptionType = error.GetType().FullName

        let stackTrace =
            Diagnostics.StackTrace(error, false).GetFrames()
            |> Option.ofObj
            |> Option.defaultValue Array.empty
            |> Array.choose (fun frame ->
                let methodInfo = frame.GetMethod()

                if isNull methodInfo then
                    None
                else
                    let declaringType =
                        methodInfo.DeclaringType
                        |> Option.ofObj
                        |> Option.bind (fun declaringType -> Option.ofObj declaringType.FullName)
                        |> Option.defaultValue "<unknown>"

                    Some $"   at {declaringType}.{methodInfo.Name}"
            )
            |> Array.truncate 12
            |> String.concat Environment.NewLine

        let stackTrace =
            if String.IsNullOrWhiteSpace stackTrace then
                "   at <stack unavailable>"
            else
                stackTrace

        let sanitizedException =
            InvalidOperationException(
                $"Lock-file comparison failed with {exceptionType}; exception details were redacted."
            )

        Runtime.ExceptionServices.ExceptionDispatchInfo.SetRemoteStackTrace(
            sanitizedException,
            stackTrace
        )

    let index: HttpHandler =
        fun next ctx ->
            let inputType =
                queryValue "input" ctx
                |> Views.InputType.parse

            let inputUrls: Views.InputUrls = {
                OlderLockFileUrl = queryValue "olderLockFileUrl" ctx
                NewerLockFileUrl = queryValue "newerLockFileUrl" ctx
                GitHubPullRequestUrl = queryValue "githubPullRequestUrl" ctx
            }

            if ctx.Request.IsHtmx then
                Views.inputSection inputType inputUrls
                |> htmlFragment
                |> fun handler -> handler next ctx
            else
                Views.page inputType inputUrls (versionInfo ())
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
                        sanitizedExceptionForLogging error,
                        "Lock-file comparison failed with {ExceptionType} ({HResult}); request {TraceIdentifier}",
                        error.GetType().FullName,
                        error.HResult,
                        ctx.TraceIdentifier
                    )

                    return!
                        Views.comparisonError error
                        |> htmlFragment
                        |> fun handler -> handler next ctx
        }
