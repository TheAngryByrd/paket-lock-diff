module Server.Tests

open Expecto

open System
open System.Collections.Generic
open System.IO
open System.Threading.Tasks
open Microsoft.AspNetCore.Http
open Microsoft.Extensions.DependencyInjection
open Microsoft.Extensions.Logging
open Microsoft.Extensions.Primitives
open Shared
open Server
open Paket
open Paket.Domain
open Server.Core
open Giraffe.ViewEngine

type private CapturedLog = {
    Level: LogLevel
    Message: string
    Error: exn
}

type private EmptyScope() =
    interface IDisposable with
        member _.Dispose() = ()

type private CapturingLogger(logs: ResizeArray<CapturedLog>) =
    interface ILogger with
        member _.BeginScope<'TState>(_: 'TState) : IDisposable = new EmptyScope()
        member _.IsEnabled _ = true

        member _.Log<'TState>
            (
                level: LogLevel,
                _: EventId,
                state: 'TState,
                error: exn,
                formatter: Func<'TState, exn, string>
            ) =
            logs.Add {
                Level = level
                Message = formatter.Invoke(state, error)
                Error = error
            }

type private CapturingLoggerFactory(logs: ResizeArray<CapturedLog>) =
    let logger = CapturingLogger(logs) :> ILogger

    interface ILoggerFactory with
        member _.AddProvider _ = ()
        member _.CreateLogger _ = logger
        member _.Dispose() = ()

let paketCompareTests =
    testList "Paket Compare" [
        let createPackage groupName packageName version =
            PaketComparer.Package.OfTuple(
                GroupName groupName,
                PackageName packageName,
                SemVer.Parse version
            )

        let createPackageVersionDiff groupName packageName olderVersion newerVersion =
            let oldVer = SemVer.Parse olderVersion
            let newVer = SemVer.Parse newerVersion

            {
                PaketComparer.PackageVersionDiff.GroupName = GroupName groupName
                PaketComparer.PackageVersionDiff.PackageName = PackageName packageName
                PaketComparer.PackageVersionDiff.OlderVersion = oldVer
                PaketComparer.PackageVersionDiff.NewerVersion = newVer
                PaketComparer.PackageVersionDiff.SemVerChange =
                    PaketComparer.calculateSemVerChange oldVer newVer
            }

        let readFile path =
            path
            |> fun relativePath -> IO.Path.Combine(__SOURCE_DIRECTORY__, relativePath)
            |> IO.File.ReadAllText
            |> String.splitByNewlines

        testCaseAsync "Additions"
        <| async {
            let older =
                "./paket-lock-files/addition-tests/old-paket.lock"
                |> readFile

            let newer =
                "./paket-lock-files/addition-tests/new-paket.lock"
                |> readFile

            let! result = PaketComparer.compare (older, newer)

            let expectedAdditions = [
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

        testCaseAsync "Removals"
        <| async {
            let older =
                "./paket-lock-files/removal-tests/old-paket.lock"
                |> readFile

            let newer =
                "./paket-lock-files/removal-tests/new-paket.lock"
                |> readFile

            let! result = PaketComparer.compare (older, newer)

            let expectedRemovals = [ createPackage "main" "FsToolkit.ErrorHandling" "1.4.3" ]
            Expect.sequenceEqual result.Additions [] ""
            Expect.sequenceEqual result.Removals expectedRemovals ""
            Expect.sequenceEqual result.VersionUpgrades [] ""
            Expect.sequenceEqual result.VersionDowngrades [] ""
        }

        testCaseAsync "Version Upgrades"
        <| async {
            let older =
                "./paket-lock-files/version-increase-tests/old-paket.lock"
                |> readFile

            let newer =
                "./paket-lock-files/version-increase-tests/new-paket.lock"
                |> readFile

            let! result = PaketComparer.compare (older, newer)

            let expectedUpgrades = [
                createPackageVersionDiff "main" "FsToolkit.ErrorHandling" "1.4.0" "1.4.3"
            ]

            Expect.sequenceEqual result.Additions [] ""
            Expect.sequenceEqual result.Removals [] ""
            Expect.sequenceEqual result.VersionUpgrades expectedUpgrades ""
            Expect.sequenceEqual result.VersionDowngrades [] ""
        }

        testCaseAsync "Version Downgrades"
        <| async {
            let older =
                "./paket-lock-files/version-decrease-tests/old-paket.lock"
                |> readFile

            let newer =
                "./paket-lock-files/version-decrease-tests/new-paket.lock"
                |> readFile

            let! result = PaketComparer.compare (older, newer)

            let expectedUpgrades = [
                createPackageVersionDiff "main" "FsToolkit.ErrorHandling" "1.4.3" "1.4.0"
            ]

            Expect.sequenceEqual result.Additions [] ""
            Expect.sequenceEqual result.Removals [] ""
            Expect.sequenceEqual result.VersionUpgrades [] ""
            Expect.sequenceEqual result.VersionDowngrades expectedUpgrades ""
        }
    ]

let viewTests =
    testList "Giraffe HTMX views" [
        testCase "Initial page renders the HTMX form and Fable enhancement module"
        <| fun _ ->
            let versionInfo = {
                PaketCore = "10.3.1"
                PaketLockDiff = "2.0.0"
            }

            let inputUrls: Views.InputUrls = {
                OlderLockFileUrl = "https://example.com/old?a=1&b=2"
                NewerLockFileUrl = ""
                GitHubPullRequestUrl = ""
            }

            let markup =
                Views.page Views.InputType.Url inputUrls versionInfo
                |> RenderView.AsString.htmlNode

            Expect.stringContains
                markup
                "hx-post=\"/compare\""
                "The comparison should post through HTMX"

            Expect.stringContains
                markup
                "hx-target=\"#comparison-results\""
                "HTMX should replace the results fragment"

            Expect.stringContains
                markup
                "hx-sync=\"#input-section:replace\""
                "A newer comparison should replace an in-flight request from any input mode"

            Expect.stringContains
                markup
                "data-fetch-mode=\"urls\""
                "The URL form should opt into Fable fetching"

            Expect.stringContains
                markup
                "data-input-panel=\"github\""
                "All input modes should remain in the DOM across tab changes"

            Expect.stringContains
                markup
                "data-input-panel=\"raw\""
                "Raw lock text should survive client-side tab changes"

            Expect.stringContains
                markup
                "/_content/Giraffe.Htmx.Common/htmx.min.js?ver=2.0.10"
                "The packaged HTMX script should be loaded"

            Expect.stringContains
                markup
                "src=\"/output/App.js\""
                "The Fable enhancement module should be loaded"

            Expect.stringContains
                markup
                "old?a=1&amp;b=2"
                "User-provided URLs should be HTML encoded"

        testCase "Comparison results include rich Markdown and JSON output"
        <| fun _ ->
            let diff: PaketDiff = {
                Additions = [
                    {
                        GroupName = "main"
                        PackageName = "Giraffe"
                        Version = "8.3.0"
                    }
                ]
                Removals = []
                VersionUpgrades = [
                    {
                        GroupName = "build"
                        PackageName = "Paket.Core"
                        OlderVersion = "9.0.2"
                        NewerVersion = "10.3.1"
                        SemVerChange = SemVerChange.Major
                    }
                ]
                VersionDowngrades = []
            }

            let markup =
                Views.results "https://example.com/report" diff
                |> RenderView.AsString.htmlNode

            Expect.stringContains
                markup
                "Additions - 1"
                "The rich view should show the addition count"

            Expect.stringContains
                markup
                "data-output-panel=\"markdown\""
                "The response should include Markdown output"

            Expect.stringContains
                markup
                "# Paket Lock Diff Report"
                "The Markdown report should be complete"

            Expect.stringContains
                markup
                "data-output-panel=\"json\""
                "The response should include JSON output"

            Expect.stringContains
                markup
                "&quot;SemVerChange&quot;: &quot;Major&quot;"
                "The JSON report should name the SemVer change"

            Expect.stringContains
                markup
                "Paket.Core - 9.0.2 -&gt; 10.3.1"
                "Untrusted result text should be encoded"

        testCase "Comparison errors are encoded and do not expose stack traces"
        <| fun _ ->
            let error =
                InvalidOperationException("<script>alert('bad')</script>", Exception("inner <bad>"))

            let markup =
                Views.comparisonError error
                |> RenderView.AsString.htmlNode

            Expect.isFalse (markup.Contains "<script>") "Error text must not be rendered as HTML"
            Expect.stringContains markup "&lt;script&gt;" "The outer error should be encoded"
            Expect.stringContains markup "inner &lt;bad&gt;" "The inner error should be encoded"

            Expect.isFalse
                (markup.Contains "StackTrace")
                "Stack traces should not be exposed to the browser"
    ]

let handlerTests =
    testList "Giraffe handlers" [
        testCaseAsync "Comparison failures log safe diagnostic exception context"
        <| async {
            let secret = "TOP_SECRET_DO_NOT_LOG"
            let logs = ResizeArray<CapturedLog>()
            use loggerFactory = new CapturingLoggerFactory(logs) :> ILoggerFactory

            use services =
                ServiceCollection()
                    .AddSingleton<ILoggerFactory>(loggerFactory)
                    .BuildServiceProvider()

            let context = DefaultHttpContext(RequestServices = services)
            context.TraceIdentifier <- "comparison-test-trace"
            context.Response.Body <- new MemoryStream()

            let form = Dictionary<string, StringValues>()
            form["olderLockFile"] <- StringValues secret

            form["newerLockFile"] <-
                StringValues
                    """STORAGE: NONE
NUGET
  remote: https://api.nuget.org/v3/index.json
    FSharp.Core (4.7.2)
"""

            context.Request.Form <- FormCollection form

            let next context = Task.FromResult(Some context)

            let! _ =
                Handlers.compare next context
                |> Async.AwaitTask

            Expect.hasLength logs 1 "A failed comparison should create one diagnostic log"
            let logged = logs[0]

            Expect.equal logged.Level LogLevel.Warning "Comparison failures should be warnings"
            Expect.isNotNull logged.Error "The warning should carry exception context"

            Expect.stringContains
                logged.Message
                context.TraceIdentifier
                "The warning should correlate to the failed request"

            Expect.stringContains
                logged.Error.Message
                "exception details were redacted"
                "The diagnostic exception should explain its safe redaction"

            Expect.isFalse
                (String.IsNullOrWhiteSpace logged.Error.StackTrace)
                "The diagnostic exception should retain method-level stack context"

            Expect.stringContains
                logged.Error.StackTrace
                "Paket"
                "The safe stack should identify the failing subsystem"

            Expect.isFalse
                ($"{logged.Message}{Environment.NewLine}{logged.Error}".Contains secret)
                "Submitted lock-file content must not be written to logs"
        }
    ]


let all =
    testList "All" [
        Shared.Tests.shared
        paketCompareTests
        viewTests
        handlerTests
    ]

[<EntryPoint>]
let main args = runTestsWithCLIArgs [] args all
