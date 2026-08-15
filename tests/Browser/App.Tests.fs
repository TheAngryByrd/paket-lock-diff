namespace Browser.Tests

open System
open System.Text.Json
open System.Threading.Tasks
open Expecto
open Microsoft.Playwright

module AppTests =

    let private awaitTask (task: Task) =
        task
        |> Async.AwaitTask

    let private locatorText (locator: ILocator) = async {
        do!
            locator.WaitForAsync()
            |> awaitTask

        return!
            locator.InnerTextAsync()
            |> Async.AwaitTask
    }

    let private expectContains (locator: ILocator) expected message = async {
        let! actual = locatorText locator
        Expect.stringContains actual expected message
    }

    let private expectVisible (locator: ILocator) message = async {
        do!
            locator.WaitForAsync()
            |> awaitTask

        let! visible =
            locator.IsVisibleAsync()
            |> Async.AwaitTask

        Expect.isTrue visible message
    }

    let private expectHidden (locator: ILocator) message = async {
        let! count =
            locator.CountAsync()
            |> Async.AwaitTask

        Expect.equal count 1 $"{message}; the element should exist exactly once"

        let! hidden =
            locator.IsHiddenAsync()
            |> Async.AwaitTask

        Expect.isTrue hidden message
    }

    let private selectInput (page: IPage) input = async {
        let tab = page.Locator($"[data-input-tab=\"{input}\"]")

        do!
            tab.ClickAsync()
            |> awaitTask

        do!
            expectVisible
                (page.Locator($"[data-input-panel=\"{input}\"]"))
                $"The {input} input panel should be selected"

        let! selected =
            tab.GetAttributeAsync "aria-selected"
            |> Async.AwaitTask

        Expect.equal selected "true" $"The {input} input tab should be active"

        let! visiblePanels =
            page.Locator("[data-input-panel]:visible").CountAsync()
            |> Async.AwaitTask

        Expect.equal visiblePanels 1 "Exactly one input mode should be visible"
    }

    let private fillAndSubmitRaw (page: IPage) olderLock newerLock = async {
        do! selectInput page "raw"

        do!
            page.Locator("#older-lock-text").FillAsync olderLock
            |> awaitTask

        do!
            page.Locator("#newer-lock-text").FillAsync newerLock
            |> awaitTask

        do!
            page.Locator("#raw-compare-form button[type=\"submit\"]").ClickAsync()
            |> awaitTask
    }

    let private submitRaw (page: IPage) olderLock newerLock = async {
        do! fillAndSubmitRaw page olderLock newerLock

        do! BrowserFixture.waitForResults page
    }

    let private routeLockFiles (page: IPage) (olderUrl: string) (newerUrl: string) = async {
        do!
            page.RouteAsync(
                olderUrl,
                fun route -> BrowserFixture.fulfillText Fixtures.OlderLockFile route
            )
            |> awaitTask

        do!
            page.RouteAsync(
                newerUrl,
                fun route -> BrowserFixture.fulfillText Fixtures.NewerLockFile route
            )
            |> awaitTask
    }

    let private waitForClientAfterReload (page: IPage) = async {
        do!
            page.Locator("html[data-paket-lock-client-ready=\"true\"]").WaitForAsync()
            |> awaitTask

        let! _ =
            page.WaitForFunctionAsync("() => typeof window.htmx !== 'undefined'")
            |> Async.AwaitTask

        return ()
    }

    let private expectQueryParameter (page: IPage) name expected message = async {
        let! values =
            page.EvaluateAsync<string[]>(
                "name => new URL(window.location.href).searchParams.getAll(name)",
                name
            )
            |> Async.AwaitTask

        Expect.equal values [| expected |] message
    }

    let all configuration =
        testSequenced
        <| testList "Original browser flows" [
            BrowserFixture.testCase
                configuration
                "landing page loads the HTMX and Fable application shell"
            <| fun page -> async {
                do! BrowserFixture.gotoReady page "/"

                let! title =
                    page.TitleAsync()
                    |> Async.AwaitTask

                Expect.equal
                    title
                    "Paket Lock Diff Tool"
                    "The original application title should remain"

                do!
                    expectVisible
                        (page.Locator("[data-input-panel=\"url\"]"))
                        "URL input should remain the default flow"

                do!
                    expectHidden
                        (page.Locator("[data-input-panel=\"github\"]"))
                        "GitHub input should initially be hidden"

                let! htmxLoaded =
                    page.EvaluateAsync<bool>("() => typeof window.htmx !== 'undefined'")
                    |> Async.AwaitTask

                Expect.isTrue htmxLoaded "The packaged HTMX client should load"

                let! appModuleCount =
                    page.Locator("script[type=\"module\"][src=\"/output/App.js\"]").CountAsync()
                    |> Async.AwaitTask

                Expect.equal appModuleCount 1 "The compiled Fable enhancement should load once"

                let! repositoryLink =
                    page
                        .Locator("nav a", PageLocatorOptions(HasText = "GitHub Repo"))
                        .GetAttributeAsync
                        "href"
                    |> Async.AwaitTask

                Expect.equal
                    repositoryLink
                    "https://github.com/TheAngryByrd/paket-lock-diff"
                    "The project navigation should remain available"

                do!
                    expectContains
                        (page.Locator "footer")
                        "Paket.Core 10.3.1"
                        "The dependency version should be visible"

                let! footer = locatorText (page.Locator "footer")

                Expect.isTrue
                    (Text.RegularExpressions.Regex.IsMatch(footer, @"paket-lock-diff \d+\.\d+\.\d+"))
                    "The application footer should contain a numeric semantic version"

                let! resultCount =
                    page.Locator("#comparison-results > *").CountAsync()
                    |> Async.AwaitTask

                Expect.equal resultCount 0 "The initial report area should be empty"
            }

            BrowserFixture.testCase
                configuration
                "URL comparison fetches both locks and restores a shared report after reload"
            <| fun page -> async {
                let mutable olderFetches = 0
                let mutable newerFetches = 0
                let mutable githubApiCalls = 0
                let mutable comparisonRequests = 0

                page.Request.Add(fun request ->
                    if
                        request.Method = "POST"
                        && request.Url.EndsWith("/compare", StringComparison.Ordinal)
                    then
                        comparisonRequests <-
                            comparisonRequests
                            + 1
                )

                do!
                    page.RouteAsync(
                        Fixtures.OlderLockUrl,
                        fun route ->
                            olderFetches <-
                                olderFetches
                                + 1

                            BrowserFixture.fulfillText Fixtures.OlderLockFile route
                    )
                    |> awaitTask

                do!
                    page.RouteAsync(
                        Fixtures.NewerLockUrl,
                        fun route ->
                            newerFetches <-
                                newerFetches
                                + 1

                            BrowserFixture.fulfillText Fixtures.NewerLockFile route
                    )
                    |> awaitTask

                do!
                    page.RouteAsync(
                        "https://api.github.com/**",
                        fun route ->
                            githubApiCalls <-
                                githubApiCalls
                                + 1

                            BrowserFixture.fulfill
                                500
                                "text/plain"
                                "unexpected GitHub API call"
                                route
                    )
                    |> awaitTask

                do! BrowserFixture.gotoReady page "/"

                do!
                    page.Locator("#older-lock-url").FillAsync Fixtures.OlderLockUrl
                    |> awaitTask

                do!
                    page.Locator("#newer-lock-url").FillAsync Fixtures.NewerLockUrl
                    |> awaitTask

                do!
                    page.Locator("#url-compare-form button[type=\"submit\"]").ClickAsync()
                    |> awaitTask

                do! BrowserFixture.waitForResults page

                do!
                    expectContains
                        (page.Locator "#comparison-results")
                        Fixtures.ExpectedUpgrade
                        "The URL flow should compare fetched bodies"

                do!
                    expectQueryParameter
                        page
                        "olderLockFileUrl"
                        Fixtures.OlderLockUrl
                        "The older URL should be shareable exactly once"

                do!
                    expectQueryParameter
                        page
                        "newerLockFileUrl"
                        Fixtures.NewerLockUrl
                        "The newer URL should be shareable exactly once"

                Expect.equal olderFetches 1 "The older lock should be fetched once"
                Expect.equal newerFetches 1 "The newer lock should be fetched once"
                Expect.equal comparisonRequests 1 "The first URL comparison should submit once"

                do! selectInput page "github"

                let! _ =
                    page.ReloadAsync(PageReloadOptions(WaitUntil = WaitUntilState.DOMContentLoaded))
                    |> Async.AwaitTask

                do! waitForClientAfterReload page
                do! BrowserFixture.waitForResults page

                do!
                    expectVisible
                        (page.Locator("[data-input-panel=\"github\"]"))
                        "The selected GitHub panel should survive a shared-link reload"

                do!
                    expectContains
                        (page.Locator "#comparison-results")
                        Fixtures.ExpectedUpgrade
                        "Reloading a shared link should auto-compare"

                Expect.equal olderFetches 2 "Reload should fetch the older URL exactly once more"
                Expect.equal newerFetches 2 "Reload should fetch the newer URL exactly once more"

                Expect.equal
                    comparisonRequests
                    2
                    "Reloading a shared URL should auto-compare exactly once"

                Expect.equal
                    githubApiCalls
                    0
                    "Discovered URL parameters should not repeat GitHub discovery"
            }

            BrowserFixture.testCase
                configuration
                "GitHub pull request comparison discovers and compares the changed root lock file"
            <| fun page -> async {
                let apiRequests = ResizeArray<IRequest>()
                let mutable comparisonRequests = 0
                let mutable nonRootContentRequests = 0

                page.Request.Add(fun request ->
                    if
                        request.Method = "POST"
                        && request.Url.EndsWith("/compare", StringComparison.Ordinal)
                    then
                        comparisonRequests <-
                            comparisonRequests
                            + 1
                )

                do!
                    page.RouteAsync(
                        Fixtures.GitHubContentsUrl,
                        fun route ->
                            apiRequests.Add route.Request

                            BrowserFixture.fulfillJson
                                $"{{\"download_url\":\"{Fixtures.OlderLockUrl}\"}}"
                                route
                    )
                    |> awaitTask

                do!
                    page.RouteAsync(
                        Fixtures.GitHubPullFilesUrl,
                        fun route ->
                            apiRequests.Add route.Request

                            BrowserFixture.fulfillJson
                                $"[{{\"filename\":\"src/paket.lock\",\"contents_url\":\"{Fixtures.GitHubNonRootContentsUrl}\"}},{{\"filename\":\"paket.lock\",\"contents_url\":\"{Fixtures.GitHubPullContentsUrl}\"}}]"
                                route
                    )
                    |> awaitTask

                do!
                    page.RouteAsync(
                        Fixtures.GitHubNonRootContentsUrl,
                        fun route ->
                            nonRootContentRequests <-
                                nonRootContentRequests
                                + 1

                            apiRequests.Add route.Request

                            BrowserFixture.fulfillJson
                                $"{{\"download_url\":\"{Fixtures.NewerLockUrl}\"}}"
                                route
                    )
                    |> awaitTask

                do!
                    page.RouteAsync(
                        Fixtures.GitHubPullContentsUrl,
                        fun route ->
                            apiRequests.Add route.Request

                            BrowserFixture.fulfillJson
                                $"{{\"download_url\":\"{Fixtures.NewerLockUrl}\"}}"
                                route
                    )
                    |> awaitTask

                do! routeLockFiles page Fixtures.OlderLockUrl Fixtures.NewerLockUrl
                do! BrowserFixture.gotoReady page "/"
                do! selectInput page "github"

                let pullRequestUrl =
                    Fixtures.GitHubPullRequestUrl
                    + ".patch"

                do!
                    page.Locator("#github-pr-url").FillAsync pullRequestUrl
                    |> awaitTask

                do!
                    page.Locator("#github-compare-form button[type=\"submit\"]").ClickAsync()
                    |> awaitTask

                do! BrowserFixture.waitForResults page

                do!
                    expectContains
                        (page.Locator "#comparison-results")
                        Fixtures.ExpectedUpgrade
                        "The GitHub flow should compare discovered locks"

                Expect.equal
                    apiRequests.Count
                    3
                    "GitHub discovery should use the contents, pull-files, and pull contents endpoints"

                Expect.equal
                    nonRootContentRequests
                    0
                    "GitHub discovery should ignore a nested paket.lock that precedes the root file"

                for request in apiRequests do
                    Expect.stringContains
                        request.Headers["accept"]
                        "application/vnd.github+json"
                        "GitHub requests should use the documented media type"

                    Expect.equal
                        request.Headers["x-github-api-version"]
                        "2022-11-28"
                        "GitHub requests should pin the API version"

                let! discoveredOlder =
                    page.Locator("#older-lock-url").InputValueAsync()
                    |> Async.AwaitTask

                let! discoveredNewer =
                    page.Locator("#newer-lock-url").InputValueAsync()
                    |> Async.AwaitTask

                Expect.equal
                    discoveredOlder
                    Fixtures.OlderLockUrl
                    "The default-branch URL should populate the URL form"

                Expect.equal
                    discoveredNewer
                    Fixtures.NewerLockUrl
                    "The pull-request URL should populate the URL form"

                do!
                    expectQueryParameter
                        page
                        "githubPullRequestUrl"
                        pullRequestUrl
                        "The pull request URL should remain shareable exactly once"

                do!
                    expectQueryParameter
                        page
                        "olderLockFileUrl"
                        Fixtures.OlderLockUrl
                        "The discovered older URL should be shareable exactly once"

                do!
                    expectQueryParameter
                        page
                        "newerLockFileUrl"
                        Fixtures.NewerLockUrl
                        "The discovered newer URL should be shareable exactly once"

                Expect.equal
                    comparisonRequests
                    1
                    "GitHub discovery should submit exactly one comparison"
            }

            BrowserFixture.testCase
                configuration
                "raw text comparison renders rich Markdown and JSON reports and preserves the selected output"
            <| fun page -> async {
                do! BrowserFixture.gotoReady page "/"
                do! submitRaw page Fixtures.OlderLockFile Fixtures.NewerLockFile

                let results = page.Locator "#comparison-results"

                do!
                    expectContains
                        results
                        "Additions - 0"
                        "The rich report should include zero-count sections"

                do! expectContains results "Removals - 0" "The rich report should include removals"

                do!
                    expectContains
                        results
                        "Version Upgrades - 1"
                        "The rich report should count upgrades"

                do!
                    expectContains
                        results
                        "Version Downgrades - 0"
                        "The rich report should include downgrades"

                do!
                    expectContains
                        results
                        Fixtures.ExpectedUpgrade
                        "The rich report should show the exact version change"

                do!
                    expectContains
                        (page.Locator("[data-output-panel=\"rich\"] .semver-tags"))
                        "Patch"
                        "The rich report should classify the upgrade as a patch"

                let! diffLink =
                    page
                        .Locator(
                            "[data-output-panel=\"rich\"] a",
                            PageLocatorOptions(HasText = "FsToolkit.ErrorHandling")
                        )
                        .GetAttributeAsync
                        "href"
                    |> Async.AwaitTask

                Expect.equal
                    diffLink
                    "https://www.fuget.org/packages/FsToolkit.ErrorHandling/1.4.3/lib/unknown/diff/1.4.0/"
                    "The rich report should preserve the package diff link"

                do!
                    page.Locator("[data-output-tab=\"markdown\"]").ClickAsync()
                    |> awaitTask

                do!
                    expectVisible
                        (page.Locator("[data-output-panel=\"markdown\"]"))
                        "Markdown output should become visible"

                let! markdown =
                    page.Locator("#markdown-output").TextContentAsync()
                    |> Async.AwaitTask

                Expect.stringContains
                    markdown
                    "# Paket Lock Diff Report"
                    "Markdown should include its report heading"

                Expect.stringContains
                    markdown
                    "## Version Upgrades - (1)"
                    "Markdown should include the upgrade section"

                Expect.stringContains
                    markdown
                    Fixtures.ExpectedUpgrade
                    "Markdown should include the exact version change"

                Expect.stringContains
                    markdown
                    page.Url
                    "Markdown should retain the shareable report URL"

                do!
                    page.Locator("[data-output-tab=\"json\"]").ClickAsync()
                    |> awaitTask

                let! json =
                    page.Locator("#json-output").TextContentAsync()
                    |> Async.AwaitTask

                use document = JsonDocument.Parse json
                let upgrades = document.RootElement.GetProperty "VersionUpgrades"
                Expect.equal (upgrades.GetArrayLength()) 1 "JSON should contain one upgrade"
                let upgrade = upgrades[0]

                Expect.equal
                    (upgrade.GetProperty("PackageName").GetString())
                    "FsToolkit.ErrorHandling"
                    "JSON should identify the package"

                Expect.equal
                    (upgrade.GetProperty("OlderVersion").GetString())
                    "1.4.0"
                    "JSON should identify the old version"

                Expect.equal
                    (upgrade.GetProperty("NewerVersion").GetString())
                    "1.4.3"
                    "JSON should identify the new version"

                Expect.equal
                    (upgrade.GetProperty("SemVerChange").GetString())
                    "Patch"
                    "JSON should preserve the semantic-version classification"

                let oldOutputRoot = page.Locator("[data-output-root]")

                let! _ =
                    oldOutputRoot.EvaluateAsync(
                        "element => element.setAttribute('data-test-old-output', 'true')"
                    )
                    |> Async.AwaitTask

                let response =
                    page.WaitForResponseAsync(fun response ->
                        response.Request.Method = "POST"
                        && response.Url.EndsWith("/compare", StringComparison.Ordinal)
                    )

                do!
                    page.Locator("#raw-compare-form button[type=\"submit\"]").ClickAsync()
                    |> awaitTask

                let! _ =
                    response
                    |> Async.AwaitTask

                do!
                    page
                        .Locator("[data-test-old-output]")
                        .WaitForAsync(LocatorWaitForOptions(State = WaitForSelectorState.Detached))
                    |> awaitTask

                do!
                    expectVisible
                        (page.Locator("[data-output-panel=\"json\"]"))
                        "A later comparison should preserve the selected output mode"

                do!
                    expectHidden
                        (page.Locator("[data-output-panel=\"rich\"]"))
                        "A later comparison should not reset the output mode to Rich"
            }

            BrowserFixture.testCase
                configuration
                "input modes preserve in-progress values through tab and history navigation"
            <| fun page -> async {
                do! BrowserFixture.gotoReady page "/"

                do!
                    page.Locator("#older-lock-url").FillAsync "https://draft.example/old.lock"
                    |> awaitTask

                do!
                    page.Locator("#newer-lock-url").FillAsync "https://draft.example/new.lock"
                    |> awaitTask

                do! selectInput page "github"

                do!
                    page.Locator("#github-pr-url").FillAsync Fixtures.GitHubPullRequestUrl
                    |> awaitTask

                do! selectInput page "raw"

                do!
                    page.Locator("#older-lock-text").FillAsync "older raw draft"
                    |> awaitTask

                do!
                    page.Locator("#newer-lock-text").FillAsync "newer raw draft"
                    |> awaitTask

                do! selectInput page "url"

                let! olderUrl =
                    page.Locator("#older-lock-url").InputValueAsync()
                    |> Async.AwaitTask

                let! newerUrl =
                    page.Locator("#newer-lock-url").InputValueAsync()
                    |> Async.AwaitTask

                Expect.equal
                    olderUrl
                    "https://draft.example/old.lock"
                    "The older URL draft should survive tab switches"

                Expect.equal
                    newerUrl
                    "https://draft.example/new.lock"
                    "The newer URL draft should survive tab switches"

                let! _ =
                    page.GoBackAsync()
                    |> Async.AwaitTask

                do!
                    expectVisible
                        (page.Locator("[data-input-panel=\"raw\"]"))
                        "Back navigation should restore the raw panel"

                let! rawOlder =
                    page.Locator("#older-lock-text").InputValueAsync()
                    |> Async.AwaitTask

                let! rawNewer =
                    page.Locator("#newer-lock-text").InputValueAsync()
                    |> Async.AwaitTask

                Expect.equal
                    rawOlder
                    "older raw draft"
                    "The older raw draft should survive history navigation"

                Expect.equal
                    rawNewer
                    "newer raw draft"
                    "The newer raw draft should survive history navigation"

                let! _ =
                    page.GoBackAsync()
                    |> Async.AwaitTask

                do!
                    expectVisible
                        (page.Locator("[data-input-panel=\"github\"]"))
                        "A second Back should restore the GitHub panel"

                let! githubUrl =
                    page.Locator("#github-pr-url").InputValueAsync()
                    |> Async.AwaitTask

                Expect.equal
                    githubUrl
                    Fixtures.GitHubPullRequestUrl
                    "The GitHub draft should survive history navigation"

                let! _ =
                    page.GoForwardAsync()
                    |> Async.AwaitTask

                do!
                    expectVisible
                        (page.Locator("[data-input-panel=\"raw\"]"))
                        "Forward navigation should restore the raw panel"

                let! visiblePanels =
                    page.Locator("[data-input-panel]:visible").CountAsync()
                    |> Async.AwaitTask

                Expect.equal visiblePanels 1 "Exactly one input mode should be visible"
            }

            BrowserFixture.testCase
                configuration
                "copy buttons copy the exact Markdown and JSON reports"
            <| fun page -> async {
                do! BrowserFixture.gotoReady page "/"
                do! submitRaw page Fixtures.OlderLockFile Fixtures.NewerLockFile

                let pageUrlBeforeCopy = page.Url

                for output in
                    [
                        "markdown"
                        "json"
                    ] do
                    do!
                        page.Locator($"[data-output-tab=\"{output}\"]").ClickAsync()
                        |> awaitTask

                    let target = page.Locator($"#{output}-output")

                    let! expected =
                        target.TextContentAsync()
                        |> Async.AwaitTask

                    let signature =
                        if output = "markdown" then
                            "# Paket Lock Diff Report"
                        else
                            "\"VersionUpgrades\""

                    Expect.stringContains
                        expected
                        signature
                        $"The {output} copy oracle should contain report content"

                    do!
                        page.Locator($"[data-copy-target=\"#{output}-output\"]").ClickAsync()
                        |> awaitTask

                    let! _ =
                        page.WaitForFunctionAsync(
                            "expected => navigator.clipboard.readText().then(value => value === expected)",
                            expected
                        )
                        |> Async.AwaitTask

                    let! actual =
                        page.EvaluateAsync<string>("() => navigator.clipboard.readText()")
                        |> Async.AwaitTask

                    Expect.equal
                        actual
                        expected
                        $"The {output} copy button should copy the rendered report exactly"

                let! outputRoots =
                    page.Locator("[data-output-root]").CountAsync()
                    |> Async.AwaitTask

                Expect.equal outputRoots 1 "Copying should not navigate or replace the comparison"

                Expect.equal page.Url pageUrlBeforeCopy "Copying should not change the report URL"
            }

            BrowserFixture.testCase
                configuration
                "clipboard failure keeps the current comparison and selected output"
            <| fun page -> async {
                do! BrowserFixture.gotoReady page "/"
                do! submitRaw page Fixtures.OlderLockFile Fixtures.NewerLockFile

                do!
                    page.Locator("[data-output-tab=\"markdown\"]").ClickAsync()
                    |> awaitTask

                let! _ =
                    page.EvaluateAsync(
                        "() => Object.defineProperty(navigator.clipboard, 'writeText', { configurable: true, value: () => Promise.reject(new Error('clipboard permission denied')) })"
                    )
                    |> Async.AwaitTask

                do!
                    page.Locator("[data-copy-target=\"#markdown-output\"]").ClickAsync()
                    |> awaitTask

                let alert = page.Locator("#comparison-results [data-client-copy-error]")

                do!
                    expectContains
                        alert
                        "Unable to copy the comparison output"
                        "A clipboard rejection should show a focused error"

                do!
                    expectContains
                        alert
                        "clipboard permission denied"
                        "The clipboard error should retain the browser's useful reason"

                let! outputRoots =
                    page.Locator("[data-output-root]").CountAsync()
                    |> Async.AwaitTask

                Expect.equal outputRoots 1 "A clipboard error must not clear the comparison"

                do!
                    expectVisible
                        (page.Locator("[data-output-panel=\"markdown\"]"))
                        "A clipboard error should preserve the selected Markdown output"

                do!
                    expectHidden
                        (page.Locator("[data-output-panel=\"rich\"]"))
                        "A clipboard error should not reset the output mode"

                do!
                    expectContains
                        (page.Locator "#comparison-results")
                        Fixtures.ExpectedUpgrade
                        "A clipboard error should preserve the rendered report"
            }

            BrowserFixture.testCaseAllowingBrowserError
                configuration
                "URL fetch failure reports the HTTP status without submitting invalid content"
                (fun error -> error.Contains("status of 503", StringComparison.Ordinal))
            <| fun page -> async {
                let mutable comparisons = 0

                do!
                    page.RouteAsync(
                        Fixtures.OlderLockUrl,
                        fun route ->
                            BrowserFixture.fulfill 503 "text/plain" "backend unavailable" route
                    )
                    |> awaitTask

                do!
                    page.RouteAsync(
                        Fixtures.NewerLockUrl,
                        fun route -> BrowserFixture.fulfillText Fixtures.NewerLockFile route
                    )
                    |> awaitTask

                do!
                    page.RouteAsync(
                        "**/compare",
                        fun route ->
                            comparisons <-
                                comparisons
                                + 1

                            route.ContinueAsync()
                    )
                    |> awaitTask

                do! BrowserFixture.gotoReady page "/"

                do!
                    page.Locator("#older-lock-url").FillAsync Fixtures.OlderLockUrl
                    |> awaitTask

                do!
                    page.Locator("#newer-lock-url").FillAsync Fixtures.NewerLockUrl
                    |> awaitTask

                do!
                    page.Locator("#url-compare-form button[type=\"submit\"]").ClickAsync()
                    |> awaitTask

                let error =
                    page.Locator("#comparison-results [data-client-fetch-error][role=\"alert\"]")

                do!
                    expectContains
                        error
                        "Unable to load the paket.lock files"
                        "The browser error should identify the failed operation"

                do! expectContains error "503" "The browser error should preserve the HTTP status"

                do!
                    expectContains
                        error
                        "backend unavailable"
                        "The browser error should preserve the useful response body"

                do!
                    expectContains
                        error
                        Fixtures.OlderLockUrl
                        "The browser error should identify the failed URL"

                Expect.equal comparisons 0 "A failed fetch must not submit invalid text to Paket"

                let! busy =
                    page.Locator("#url-compare-form").GetAttributeAsync "aria-busy"
                    |> Async.AwaitTask

                Expect.isNull busy "The URL form should leave its busy state after an error"

                let! buttonEnabled =
                    page.Locator("#url-compare-form button[type=\"submit\"]").IsEnabledAsync()
                    |> Async.AwaitTask

                Expect.isTrue buttonEnabled "The URL flow should be retryable after an error"
            }

            BrowserFixture.testCase
                configuration
                "invalid raw lock text shows a safe comparison error"
            <| fun page -> async {
                do! BrowserFixture.gotoReady page "/"

                let maliciousLock =
                    "TOP_SECRET_<script id=\"injected\">window.__paketInjected = true</script>"

                do! fillAndSubmitRaw page maliciousLock Fixtures.NewerLockFile

                let alert = page.Locator("#comparison-results [role=\"alert\"]")

                do!
                    expectContains
                        alert
                        "Unable to compare these lock files"
                        "Malformed input should produce an actionable heading"

                do!
                    expectContains
                        alert
                        "TOP_SECRET"
                        "The parser context should be shown as encoded text"

                let! injectedElements =
                    page.Locator("#injected").CountAsync()
                    |> Async.AwaitTask

                Expect.equal
                    injectedElements
                    0
                    "Malformed lock text must not create executable markup"

                let! injected =
                    page.EvaluateAsync<bool>("() => window.__paketInjected === true")
                    |> Async.AwaitTask

                Expect.isFalse injected "Malformed lock text must not execute"

                let! alertHtml =
                    alert.InnerHTMLAsync()
                    |> Async.AwaitTask

                Expect.isFalse
                    (alertHtml.Contains("<script", StringComparison.OrdinalIgnoreCase))
                    "The response should encode script markup"

                Expect.isFalse
                    (alertHtml.Contains("Server.PaketComparer", StringComparison.Ordinal))
                    "The response should not expose server stack frames"
            }

            BrowserFixture.testCase
                configuration
                "a newer raw comparison wins over a delayed URL fetch"
            <| fun page -> async {
                let releaseRemoteFetch =
                    TaskCompletionSource<unit>(TaskCreationOptions.RunContinuationsAsynchronously)

                do!
                    page.RouteAsync(
                        Fixtures.OlderLockUrl,
                        fun route ->
                            (task {
                                do! releaseRemoteFetch.Task
                                do! BrowserFixture.fulfillText Fixtures.OlderLockFile route
                            }
                            :> Task)
                    )
                    |> awaitTask

                do!
                    page.RouteAsync(
                        Fixtures.NewerLockUrl,
                        fun route ->
                            (task {
                                do! releaseRemoteFetch.Task
                                do! BrowserFixture.fulfillText Fixtures.NewerLockFile route
                            }
                            :> Task)
                    )
                    |> awaitTask

                do! BrowserFixture.gotoReady page "/"

                do!
                    page.Locator("#older-lock-url").FillAsync Fixtures.OlderLockUrl
                    |> awaitTask

                do!
                    page.Locator("#newer-lock-url").FillAsync Fixtures.NewerLockUrl
                    |> awaitTask

                do!
                    page.Locator("#url-compare-form button[type=\"submit\"]").ClickAsync()
                    |> awaitTask

                let! _ =
                    page.WaitForFunctionAsync(
                        "() => document.querySelector('#url-compare-form').getAttribute('aria-busy') === 'true'"
                    )
                    |> Async.AwaitTask

                do!
                    submitRaw
                        page
                        Fixtures.AlternativeOlderLockFile
                        Fixtures.AlternativeNewerLockFile

                do!
                    expectContains
                        (page.Locator "#comparison-results")
                        "FSharp.Core - 4.7.1 -> 4.7.2"
                        "The newer raw comparison should be visible"

                releaseRemoteFetch.SetResult()

                let! _ =
                    page.WaitForFunctionAsync(
                        "() => !document.querySelector('#url-compare-form').hasAttribute('aria-busy')"
                    )
                    |> Async.AwaitTask

                let! staleOlderValue =
                    page.Locator("#url-compare-form [name=\"olderLockFile\"]").InputValueAsync()
                    |> Async.AwaitTask

                let! staleNewerValue =
                    page.Locator("#url-compare-form [name=\"newerLockFile\"]").InputValueAsync()
                    |> Async.AwaitTask

                Expect.equal
                    staleOlderValue
                    ""
                    "A stale fetch should not populate its hidden older lock"

                Expect.equal
                    staleNewerValue
                    ""
                    "A stale fetch should not populate its hidden newer lock"

                let! results =
                    page.Locator("#comparison-results").InnerTextAsync()
                    |> Async.AwaitTask

                Expect.stringContains
                    results
                    "FSharp.Core - 4.7.1 -> 4.7.2"
                    "The newer result should remain visible"

                Expect.isFalse
                    (results.Contains Fixtures.ExpectedUpgrade)
                    "The stale URL result must not overwrite the newer raw result"
            }

            BrowserFixture.testCaseAllowingBrowserError
                configuration
                "a delayed URL error cannot replace a newer raw comparison"
                (fun error -> error.Contains("status of 503", StringComparison.Ordinal))
            <| fun page -> async {
                let releaseRemoteFetch =
                    TaskCompletionSource<unit>(TaskCreationOptions.RunContinuationsAsynchronously)

                do!
                    page.RouteAsync(
                        Fixtures.OlderLockUrl,
                        fun route ->
                            (task {
                                do! releaseRemoteFetch.Task

                                do!
                                    BrowserFixture.fulfill
                                        503
                                        "text/plain"
                                        "delayed backend failure"
                                        route
                            }
                            :> Task)
                    )
                    |> awaitTask

                do!
                    page.RouteAsync(
                        Fixtures.NewerLockUrl,
                        fun route -> BrowserFixture.fulfillText Fixtures.NewerLockFile route
                    )
                    |> awaitTask

                do! BrowserFixture.gotoReady page "/"

                do!
                    page.Locator("#older-lock-url").FillAsync Fixtures.OlderLockUrl
                    |> awaitTask

                do!
                    page.Locator("#newer-lock-url").FillAsync Fixtures.NewerLockUrl
                    |> awaitTask

                do!
                    page.Locator("#url-compare-form button[type=\"submit\"]").ClickAsync()
                    |> awaitTask

                let! _ =
                    page.WaitForFunctionAsync(
                        "() => document.querySelector('#url-compare-form').getAttribute('aria-busy') === 'true'"
                    )
                    |> Async.AwaitTask

                do!
                    submitRaw
                        page
                        Fixtures.AlternativeOlderLockFile
                        Fixtures.AlternativeNewerLockFile

                releaseRemoteFetch.SetResult()

                let! _ =
                    page.WaitForFunctionAsync(
                        "() => !document.querySelector('#url-compare-form').hasAttribute('aria-busy')"
                    )
                    |> Async.AwaitTask

                let! fetchErrors =
                    page.Locator("[data-client-fetch-error]").CountAsync()
                    |> Async.AwaitTask

                Expect.equal fetchErrors 0 "A stale fetch error should be ignored"

                let! outputRoots =
                    page.Locator("[data-output-root]").CountAsync()
                    |> Async.AwaitTask

                Expect.equal outputRoots 1 "A stale fetch error must not clear the newer report"

                let! results =
                    page.Locator("#comparison-results").InnerTextAsync()
                    |> Async.AwaitTask

                Expect.stringContains
                    results
                    "FSharp.Core - 4.7.1 -> 4.7.2"
                    "The newer raw comparison should remain visible after a stale error"

                Expect.isFalse
                    (results.Contains "delayed backend failure")
                    "The stale URL failure must not replace the newer raw result"
            }
        ]
