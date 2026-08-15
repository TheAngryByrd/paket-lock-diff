module Index

open System
open Browser.Dom
open Browser.Types
open Fable.Core
open Fable.Core.JsInterop
open Fetch

[<Literal>]
let LocksLoadedEvent = "paket-locks-loaded"

[<Literal>]
let OlderLockFileUrlQueryParam = "olderLockFileUrl"

[<Literal>]
let NewerLockFileUrlQueryParam = "newerLockFileUrl"

[<Literal>]
let GitHubPullRequestUrlQueryParam = "githubPullRequestUrl"

[<Literal>]
let InputTypeQueryParam = "input"

[<Literal>]
let private ComparisonGenerationAttribute = "data-comparison-generation"

[<Literal>]
let private FormGenerationAttribute = "data-fetch-generation"

let private httpFailureMessage url (response: Response) body =
    let status =
        if String.IsNullOrWhiteSpace response.StatusText then
            string response.Status
        else
            $"{response.Status} {response.StatusText}"

    let responseDetails =
        if String.IsNullOrWhiteSpace body then
            ""
        else
            let trimmedBody = body.Trim()

            let abbreviatedBody =
                if trimmedBody.Length > 500 then
                    trimmedBody.Substring(0, 500)
                    + "..."
                else
                    trimmedBody

            $" Response: {abbreviatedBody}"

    $"Request to {url} failed with HTTP {status}.{responseDetails}"

module GitHub =
    type PullRequestInfo = {
        Owner: string
        Repository: string
        Number: string
    }

    type private Contents = { download_url: string }

    type private PullRequestFile = {
        filename: string
        contents_url: string
    }

    let private isAsciiLetterOrDigit character =
        (character
         >= 'a'
         && character
            <= 'z')
        || (character
            >= 'A'
            && character
               <= 'Z')
        || (character
            >= '0'
            && character
               <= '9')

    let private isOwnerCharacter character =
        isAsciiLetterOrDigit character
        || character = '-'

    let private isRepositoryCharacter character =
        isAsciiLetterOrDigit character
        || character = '-'
        || character = '_'
        || character = '.'

    let private isHexadecimalDigit character =
        (character
         >= '0'
         && character
            <= '9')
        || (character
            >= 'a'
            && character
               <= 'f')
        || (character
            >= 'A'
            && character
               <= 'F')

    let private hasWellFormedPercentEscapes (value: string) =
        let rec check index =
            if
                index
                >= value.Length
            then
                true
            elif
                value.[index]
                <> '%'
            then
                check (index + 1)
            elif
                index + 2 < value.Length
                && isHexadecimalDigit value.[index + 1]
                && isHexadecimalDigit value.[index + 2]
            then
                check (index + 3)
            else
                false

        check 0

    let private isValidSegment isAllowed (value: string) =
        not (String.IsNullOrWhiteSpace value)
        && value
           |> Seq.forall isAllowed

    let private isPositiveNumber (value: string) =
        not (String.IsNullOrWhiteSpace value)
        && value
           |> Seq.forall (fun character ->
               character
               >= '0'
               && character
                  <= '9'
           )
        && value
           |> Seq.exists ((<>) '0')

    let private normalizePullRequestNumber (value: string) =
        [
            ".patch"
            ".diff"
        ]
        |> List.tryPick (fun suffix ->
            if value.EndsWith(suffix, StringComparison.OrdinalIgnoreCase) then
                Some(
                    value.Substring(
                        0,
                        value.Length
                        - suffix.Length
                    )
                )
            else
                None
        )
        |> Option.defaultValue value

    let private tryDecodePathSegments (uri: Uri) =
        try
            Uri.UnescapeDataString uri.Query
            |> ignore

            Uri.UnescapeDataString uri.Fragment
            |> ignore

            uri.AbsolutePath.Split('/')
            |> Array.filter (
                String.IsNullOrWhiteSpace
                >> not
            )
            |> Array.map Uri.UnescapeDataString
            |> Ok
        with _ ->
            Error "The GitHub pull request URL contains an invalid escape sequence."

    let private parsePullRequestUri (trimmedValue: string) (uri: Uri) =
        let hasSupportedScheme =
            String.Equals(uri.Scheme, "http", StringComparison.OrdinalIgnoreCase)
            || String.Equals(uri.Scheme, "https", StringComparison.OrdinalIgnoreCase)

        let hasGitHubHost =
            String.Equals(uri.Host, "github.com", StringComparison.OrdinalIgnoreCase)
            || String.Equals(uri.Host, "www.github.com", StringComparison.OrdinalIgnoreCase)

        let authorityStart =
            trimmedValue.IndexOf("://", StringComparison.Ordinal)
            + 3

        let authorityEnd =
            [|
                '/'
                '?'
                '#'
            |]
            |> Array.map (fun separator ->
                let index = trimmedValue.IndexOf(separator, authorityStart)
                if index < 0 then trimmedValue.Length else index
            )
            |> Array.min

        let hasUserInfo =
            authorityStart
            >= 3
            && trimmedValue
                .Substring(
                    authorityStart,
                    authorityEnd
                    - authorityStart
                )
                .Contains('@')

        if
            not hasSupportedScheme
            || not hasGitHubHost
            || not uri.IsDefaultPort
            || hasUserInfo
        then
            Error "Enter a pull request URL hosted on github.com."
        else
            match tryDecodePathSegments uri with
            | Error error -> Error error
            | Ok segments ->
                if
                    segments.Length < 4
                    || segments.[2]
                       <> "pull"
                then
                    Error
                        "The URL must have the form https://github.com/{owner}/{repository}/pull/{number}."
                elif
                    not (isValidSegment isOwnerCharacter segments.[0])
                    || segments.[0].StartsWith('-')
                    || segments.[0].EndsWith('-')
                then
                    Error "The GitHub owner in the pull request URL is invalid."
                elif
                    not (isValidSegment isRepositoryCharacter segments.[1])
                    || segments.[1] = "."
                    || segments.[1] = ".."
                then
                    Error "The GitHub repository in the pull request URL is invalid."
                else
                    let pullRequestNumber = normalizePullRequestNumber segments.[3]

                    if not (isPositiveNumber pullRequestNumber) then
                        Error "The pull request number must be a positive integer."
                    else
                        Ok {
                            Owner = segments.[0]
                            Repository = segments.[1]
                            Number = pullRequestNumber
                        }

    /// Parses a browser URL for a GitHub pull request. Query strings, fragments,
    /// trailing slashes, and pull-request sub-pages such as /files are accepted.
    let tryParsePullRequestUrl (value: string) : Result<PullRequestInfo, string> =
        if String.IsNullOrWhiteSpace value then
            Error "Enter a GitHub pull request URL."
        else
            let trimmedValue = value.Trim()

            if not (hasWellFormedPercentEscapes trimmedValue) then
                Error "The GitHub pull request URL contains an invalid escape sequence."
            else
                match Uri.TryCreate(trimmedValue, UriKind.Absolute) with
                | false, _ -> Error "Enter an absolute GitHub pull request URL."
                | true, uri -> parsePullRequestUri trimmedValue uri

    let private githubHeaders =
        requestHeaders [
            HttpRequestHeaders.Accept "application/vnd.github+json"
            HttpRequestHeaders.Custom("X-GitHub-Api-Version", "2022-11-28")
        ]

    let private fetchJson<'value> url = async {
        let! response =
            fetchUnsafe url [ githubHeaders ]
            |> Async.AwaitPromise

        if response.Ok then
            return!
                response.json<'value> ()
                |> Async.AwaitPromise
        else
            let! body =
                response.text ()
                |> Async.AwaitPromise

            return failwith (httpFailureMessage url response body)
    }

    let private requireDownloadUrl description (contents: Contents) =
        if String.IsNullOrWhiteSpace contents.download_url then
            failwith $"GitHub did not provide a download URL for the {description} paket.lock."

        contents.download_url

    /// Finds the default-branch and pull-request versions of paket.lock by using
    /// the same GitHub contents and pull-files APIs as the original client.
    let discoverLockUrls info = async {
        let repositoryApi = $"https://api.github.com/repos/{info.Owner}/{info.Repository}"

        let! olderContents = fetchJson<Contents> $"{repositoryApi}/contents/paket.lock"

        let! pullRequestFiles =
            fetchJson<PullRequestFile array>
                $"{repositoryApi}/pulls/{info.Number}/files?per_page=100"

        let pullRequestLock =
            pullRequestFiles
            |> Array.tryFind (fun file -> file.filename = "paket.lock")
            |> Option.defaultWith (fun () ->
                failwith "The pull request does not contain a root-level paket.lock file."
            )

        let! newerContents = fetchJson<Contents> pullRequestLock.contents_url

        return
            requireDownloadUrl "default branch" olderContents,
            requireDownloadUrl "pull request" newerContents
    }

module private BrowserInterop =
    [<Emit("new URLSearchParams($0)")>]
    let createQueryParameters (_queryString: string) : obj = jsNative

    [<Emit("$0.get($1)")>]
    let getQueryParameter (_parameters: obj, _name: string) : string = jsNative

    [<Emit("$0.set($1, $2)")>]
    let setQueryParameter (_parameters: obj, _name: string, _value: string) : unit = jsNative

    [<Emit("$0.toString()")>]
    let queryString (_parameters: obj) : string = jsNative

    [<Emit("$0 instanceof HTMLFormElement ? $0 : null")>]
    let eventForm (_target: obj) : HTMLFormElement = jsNative

    [<Emit("$0.target instanceof Element ? $0.target.closest($1) : null")>]
    let closestFromEvent (_event: Event, _selector: string) : Element = jsNative

    [<Emit("Array.from(document.querySelectorAll($0))")>]
    let querySelectorAll (_selector: string) : Element array = jsNative

    [<Emit("Array.from($0.querySelectorAll($1))")>]
    let querySelectorAllWithin (_element: Element, _selector: string) : Element array = jsNative

    [<Emit("$0.value")>]
    let value (_element: Element) : string = jsNative

    [<Emit("$0.value = $1")>]
    let setValue (_element: Element, _value: string) : unit = jsNative

    [<Emit("$0.disabled = $1")>]
    let setDisabled (_element: Element, _disabled: bool) : unit = jsNative

    [<Emit("$0.hidden = $1")>]
    let setHidden (_element: Element, _hidden: bool) : unit = jsNative

    [<Emit("($0.closest('li') || $0).classList.toggle('is-active', $1)")>]
    let setTabActive (_element: Element, _active: bool) : unit = jsNative

    [<Emit("$0.dispatchEvent(new CustomEvent($1, { bubbles: true }))")>]
    let dispatchCustomEvent (_element: Element, _eventName: string) : unit = jsNative

    [<Emit("navigator.clipboard.writeText($0)")>]
    let writeClipboardText (_text: string) : JS.Promise<unit> = jsNative

    [<Emit("('value' in $0) ? $0.value : ($0.textContent || '')")>]
    let copyableText (_element: Element) : string = jsNative

    [<Emit("$0.remove()")>]
    let remove (_element: Element) : unit = jsNative

    [<Emit("$0.delete($1)")>]
    let deleteQueryParameter (_parameters: obj, _name: string) : unit = jsNative

    [<Emit("history.replaceState(history.state, '', $0)")>]
    let replaceCurrentUrl (_relativeUrl: string) : unit = jsNative

    [<Emit("history.pushState({ paketLockClient: true }, '', $0)")>]
    let pushClientUrl (_relativeUrl: string) : unit = jsNative

    [<Emit("$0.requestSubmit()")>]
    let requestSubmit (_form: HTMLFormElement) : unit = jsNative

    [<Emit("$0.addEventListener($1, $2)")>]
    let addEventListener (_target: obj, _eventName: string, _handler: Event -> unit) : unit =
        jsNative

    [<Emit("$0.addEventListener($1, $2, true)")>]
    let addCapturingEventListener
        (_target: obj, _eventName: string, _handler: Event -> unit)
        : unit =
        jsNative

module private Client =
    let private tryFind selector =
        let element = document.querySelector selector

        if isNullOrUndefined element then None else Some element

    let private findResultsContainer () =
        tryFind "#comparison-results"
        |> Option.orElseWith (fun () -> tryFind "#results")

    let private nextComparisonGeneration () =
        let root = document.documentElement

        let currentGeneration =
            match Int64.TryParse(root.getAttribute ComparisonGenerationAttribute) with
            | true, generation -> generation
            | false, _ -> 0L

        let generation =
            string (
                currentGeneration
                + 1L
            )

        root.setAttribute (ComparisonGenerationAttribute, generation)
        generation

    let private isCurrentComparison generation =
        document.documentElement.getAttribute ComparisonGenerationAttribute = generation

    let private clearClientErrors () =
        match findResultsContainer () with
        | None -> ()
        | Some container ->
            BrowserInterop.querySelectorAllWithin (container, "[data-client-error]")
            |> Array.iter BrowserInterop.remove

    let private showError clearResults errorAttribute summary (error: exn) =
        match findResultsContainer () with
        | None -> console.error error
        | Some container ->
            if clearResults then
                container.innerHTML <- ""
            else
                let previousError = container.querySelector $"[{errorAttribute}]"

                if not (isNullOrUndefined previousError) then
                    BrowserInterop.remove previousError

            let notification = document.createElement "div"
            notification.className <- "notification is-danger"
            notification.setAttribute ("data-client-error", "")
            notification.setAttribute (errorAttribute, "")
            notification.setAttribute ("role", "alert")
            notification.textContent <- $"{summary} {error.Message}"

            container.appendChild notification
            |> ignore

    let private showFetchError error =
        showError true "data-client-fetch-error" "Unable to load the paket.lock files." error

    let private showClipboardError error =
        showError false "data-client-copy-error" "Unable to copy the comparison output." error

    let private requireNamedField (form: HTMLFormElement) name =
        let field = form.querySelector $"[name=\"{name}\"]"

        if isNullOrUndefined field then
            failwith $"The form is missing its {name} field."

        field

    let private requiredValue form name =
        let value =
            (requireNamedField form name
             |> BrowserInterop.value)
                .Trim()

        if String.IsNullOrWhiteSpace value then
            failwith $"The {name} field is required."

        value

    let private setNamedValue form name value =
        requireNamedField form name
        |> fun field -> BrowserInterop.setValue (field, value)

    let private setAllNamedValues name value =
        BrowserInterop.querySelectorAll $"[name=\"{name}\"]"
        |> Array.iter (fun field -> BrowserInterop.setValue (field, value))

    let private relativeUrlWith parametersToUpdate =
        let query = BrowserInterop.createQueryParameters window.location.search

        for name, value in parametersToUpdate do
            if String.IsNullOrWhiteSpace value then
                BrowserInterop.deleteQueryParameter (query, name)
            else
                BrowserInterop.setQueryParameter (query, name, value)

        let queryString = BrowserInterop.queryString query

        let relativeUrl =
            window.location.pathname
            + (if queryString = "" then "" else $"?{queryString}")
            + window.location.hash

        relativeUrl

    let private updateHistory parametersToUpdate =
        parametersToUpdate
        |> relativeUrlWith
        |> BrowserInterop.replaceCurrentUrl

    let private pushHistory parametersToUpdate =
        parametersToUpdate
        |> relativeUrlWith
        |> BrowserInterop.pushClientUrl

    let private fetchText url = async {
        let! response =
            fetchUnsafe url []
            |> Async.AwaitPromise

        let! body =
            response.text ()
            |> Async.AwaitPromise

        if response.Ok then
            return body
        else
            return failwith (httpFailureMessage url response body)
    }

    let private fetchBoth olderUrl newerUrl = async {
        let! lockFiles =
            [|
                fetchText olderUrl
                fetchText newerUrl
            |]
            |> Async.Parallel

        return lockFiles.[0], lockFiles.[1]
    }

    let private finishFetching generation form olderLockFile newerLockFile =
        if isCurrentComparison generation then
            setNamedValue form "olderLockFile" olderLockFile
            setNamedValue form "newerLockFile" newerLockFile
            BrowserInterop.dispatchCustomEvent (form, LocksLoadedEvent)

    let private submitUrlForm generation form = async {
        let olderUrl = requiredValue form OlderLockFileUrlQueryParam
        let newerUrl = requiredValue form NewerLockFileUrlQueryParam

        updateHistory [
            OlderLockFileUrlQueryParam, olderUrl
            NewerLockFileUrlQueryParam, newerUrl
        ]

        setAllNamedValues OlderLockFileUrlQueryParam olderUrl
        setAllNamedValues NewerLockFileUrlQueryParam newerUrl

        let! olderLockFile, newerLockFile = fetchBoth olderUrl newerUrl
        finishFetching generation form olderLockFile newerLockFile
    }

    let private submitGitHubForm generation form = async {
        let pullRequestUrl = requiredValue form GitHubPullRequestUrlQueryParam
        updateHistory [ GitHubPullRequestUrlQueryParam, pullRequestUrl ]

        let pullRequest =
            GitHub.tryParsePullRequestUrl pullRequestUrl
            |> Result.defaultWith failwith

        let! olderUrl, newerUrl = GitHub.discoverLockUrls pullRequest

        if isCurrentComparison generation then
            updateHistory [
                OlderLockFileUrlQueryParam, olderUrl
                NewerLockFileUrlQueryParam, newerUrl
            ]

            setAllNamedValues OlderLockFileUrlQueryParam olderUrl
            setAllNamedValues NewerLockFileUrlQueryParam newerUrl

        let! olderLockFile, newerLockFile = fetchBoth olderUrl newerUrl
        finishFetching generation form olderLockFile newerLockFile
    }

    let private setFormBusy (form: HTMLFormElement) busy =
        if busy then
            form.setAttribute ("aria-busy", "true")
        else
            form.removeAttribute "aria-busy"

        BrowserInterop.querySelectorAllWithin (
            form,
            "button[type=\"submit\"], input[type=\"submit\"]"
        )
        |> Array.iter (fun button -> BrowserInterop.setDisabled (button, busy))

    let private startSubmission (form: HTMLFormElement) operation =
        let generation = nextComparisonGeneration ()
        form.setAttribute (FormGenerationAttribute, generation)
        setFormBusy form true
        clearClientErrors ()

        async {
            try
                try
                    do! operation generation form
                with error ->
                    if isCurrentComparison generation then
                        showFetchError error
            finally
                if form.getAttribute FormGenerationAttribute = generation then
                    form.removeAttribute FormGenerationAttribute
                    setFormBusy form false
        }
        |> Async.StartImmediate

    let handleSubmit (event: Event) =
        let form = BrowserInterop.eventForm event.target

        if not (isNullOrUndefined form) then
            match form.getAttribute "data-fetch-mode" with
            | "urls" ->
                event.preventDefault ()

                if
                    form.getAttribute "aria-busy"
                    <> "true"
                    && form.reportValidity ()
                then
                    startSubmission form submitUrlForm
            | "github" ->
                event.preventDefault ()

                if
                    form.getAttribute "aria-busy"
                    <> "true"
                    && form.reportValidity ()
                then
                    startSubmission form submitGitHubForm
            | _ ->
                if form.getAttribute "hx-target" = "#comparison-results" then
                    nextComparisonGeneration ()
                    |> ignore

    let private switchOutput selectedOutput =
        BrowserInterop.querySelectorAll "[data-output-tab]"
        |> Array.iter (fun tab ->
            let isSelected = tab.getAttribute "data-output-tab" = selectedOutput
            BrowserInterop.setTabActive (tab, isSelected)
            tab.setAttribute ("aria-selected", if isSelected then "true" else "false")
        )

        BrowserInterop.querySelectorAll "[data-output-panel]"
        |> Array.iter (fun panel ->
            let isSelected = panel.getAttribute "data-output-panel" = selectedOutput
            BrowserInterop.setHidden (panel, not isSelected)
            panel.setAttribute ("aria-hidden", if isSelected then "false" else "true")
        )

    let private copyOutput (button: Element) =
        let selector = button.getAttribute "data-copy-target"

        if String.IsNullOrWhiteSpace selector then
            failwith "The copy button does not identify an output element."

        let output = document.querySelector selector

        if isNullOrUndefined output then
            failwith $"The copy target {selector} was not found."

        findResultsContainer ()
        |> Option.map (fun container -> container.querySelector "[data-client-copy-error]")
        |> Option.filter (
            isNullOrUndefined
            >> not
        )
        |> Option.iter BrowserInterop.remove

        async {
            try
                do!
                    BrowserInterop.copyableText output
                    |> BrowserInterop.writeClipboardText
                    |> Async.AwaitPromise
            with error ->
                showClipboardError error
        }
        |> Async.StartImmediate

    let private normalizeInputType =
        function
        | "github" -> "github"
        | "raw" -> "raw"
        | _ -> "url"

    let private selectedInputFromQueryString () =
        let query = BrowserInterop.createQueryParameters window.location.search

        BrowserInterop.getQueryParameter (query, InputTypeQueryParam)
        |> normalizeInputType

    let private switchInput selectedInput =
        let selectedInput = normalizeInputType selectedInput

        BrowserInterop.querySelectorAll "[data-input-tab]"
        |> Array.iter (fun tab ->
            let isSelected = tab.getAttribute "data-input-tab" = selectedInput
            BrowserInterop.setTabActive (tab, isSelected)
            tab.setAttribute ("aria-selected", if isSelected then "true" else "false")
        )

        BrowserInterop.querySelectorAll "[data-input-panel]"
        |> Array.iter (fun panel ->
            let isSelected = panel.getAttribute "data-input-panel" = selectedInput
            BrowserInterop.setHidden (panel, not isSelected)
            panel.setAttribute ("aria-hidden", if isSelected then "false" else "true")
        )

    let private queryBackedInputValues () =
        [
            OlderLockFileUrlQueryParam
            NewerLockFileUrlQueryParam
            GitHubPullRequestUrlQueryParam
        ]
        |> List.choose (fun name ->
            match tryFind $"[name=\"{name}\"]" with
            | None -> None
            | Some field -> Some(name, (BrowserInterop.value field).Trim())
        )

    let private selectInput selectedInput =
        let selectedInput = normalizeInputType selectedInput
        updateHistory (queryBackedInputValues ())

        if
            selectedInput
            <> selectedInputFromQueryString ()
        then
            pushHistory [ InputTypeQueryParam, selectedInput ]

        switchInput selectedInput

    let handleClick (event: Event) =
        let inputTab = BrowserInterop.closestFromEvent (event, "[data-input-tab]")

        if not (isNullOrUndefined inputTab) then
            event.preventDefault ()
            selectInput (inputTab.getAttribute "data-input-tab")
        else
            let outputTab = BrowserInterop.closestFromEvent (event, "[data-output-tab]")

            if not (isNullOrUndefined outputTab) then
                event.preventDefault ()
                switchOutput (outputTab.getAttribute "data-output-tab")
            else
                let copyButton = BrowserInterop.closestFromEvent (event, "[data-copy-target]")

                if not (isNullOrUndefined copyButton) then
                    event.preventDefault ()

                    try
                        copyOutput copyButton
                    with error ->
                        showClipboardError error

    let hydrateInputsFromQueryString () =
        let query = BrowserInterop.createQueryParameters window.location.search

        [
            OlderLockFileUrlQueryParam
            NewerLockFileUrlQueryParam
            GitHubPullRequestUrlQueryParam
        ]
        |> List.iter (fun name ->
            let value = BrowserInterop.getQueryParameter (query, name)

            if not (isNullOrUndefined value) then
                BrowserInterop.querySelectorAll $"[name=\"{name}\"]"
                |> Array.iter (fun field -> BrowserInterop.setValue (field, value))
        )

    let private autoCompareSharedUrls () =
        let root = document.documentElement
        let query = BrowserInterop.createQueryParameters window.location.search
        let olderUrl = BrowserInterop.getQueryParameter (query, OlderLockFileUrlQueryParam)
        let newerUrl = BrowserInterop.getQueryParameter (query, NewerLockFileUrlQueryParam)

        if
            root.getAttribute "data-shared-url-auto-submitted"
            <> "true"
            && not (String.IsNullOrWhiteSpace olderUrl)
            && not (String.IsNullOrWhiteSpace newerUrl)
        then
            match tryFind "form[data-fetch-mode=\"urls\"]" with
            | None -> ()
            | Some form ->
                root.setAttribute ("data-shared-url-auto-submitted", "true")
                BrowserInterop.requestSubmit (unbox<HTMLFormElement> form)

    let attach () =
        let root = document.documentElement

        if
            root.getAttribute "data-paket-lock-client-ready"
            <> "true"
        then
            root.setAttribute ("data-paket-lock-client-ready", "true")
            BrowserInterop.addCapturingEventListener (document, "submit", handleSubmit)
            BrowserInterop.addEventListener (document, "click", handleClick)

            BrowserInterop.addEventListener (
                document,
                "htmx:afterSwap",
                fun _ -> hydrateInputsFromQueryString ()
            )

            BrowserInterop.addEventListener (
                window,
                "popstate",
                fun _ ->
                    hydrateInputsFromQueryString ()
                    switchInput (selectedInputFromQueryString ())
            )

            hydrateInputsFromQueryString ()
            switchInput (selectedInputFromQueryString ())
            autoCompareSharedUrls ()

let initialize () =
    if document.readyState = "loading" then
        BrowserInterop.addEventListener (document, "DOMContentLoaded", fun _ -> Client.attach ())
    else
        Client.attach ()
