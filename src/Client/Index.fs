module Index

open Elmish
open Fable.Core
open Fable.Core.JsInterop
open Fable.Remoting.Client
open Shared
open Fetch
open System
open Browser

type PaketLockFile = string

type CompareResults =
    | Finished of Shared.PaketDiff
    | Loading
    | NotStarted
    | Errored of exn

[<RequireQualifiedAccess>]
type InputType =
    | RawText
    | Url
    | GitHubPR

    member x.IsRawText2 =
        match x with
        | RawText -> true
        | _ -> false

    member x.IsUrl2 =
        match x with
        | Url -> true
        | _ -> false

    member x.IsGitHubPR2 =
        match x with
        | GitHubPR -> true
        | _ -> false


module Json =
    [<Emit("JSON.stringify($1, null, $0)")>]
    let pretty (_space: int, _value: obj) : string = jsNative


module Markdown =
    let heading1 (v: string) =
        Fable.React.Helpers.str <| sprintf "# %s\n\n" v

    let heading2 (v: string) =
        Fable.React.Helpers.str <| sprintf "## %s\n\n" v

    let lii (indentSize: int) (v: string) =
        let indent = List.init indentSize (fun _ -> "\u00A0") |> String.concat ""

        Fable.React.Helpers.str <| sprintf "%s* %s\n" indent v

    let li (v: string) = lii 0 v

type OutputType =
    | Rich
    | Markdown
    | Json

    member x.IsRich2 =
        match x with
        | Rich -> true
        | _ -> false

    member x.IsMarkdown2 =
        match x with
        | Markdown -> true
        | _ -> false

    member x.IsJson2 =
        match x with
        | Json -> true
        | _ -> false

module Fetcher =
    let getFromUrl (url) = async {
        let! result = fetch url [] |> Async.AwaitPromise

        if result.Ok then
            let! body = result.text () |> Async.AwaitPromise

            return Ok body
        else
            let! body = result.text () |> Async.AwaitPromise

            return Error(Exception(body))
    }

module GitHub =
    open System.Text.RegularExpressions
    open Thoth.Fetch

    type Contents = { download_url: string }

    type PRFiles = {
        filename: string
        contents_url: string
    }

    type PRInfo = {
        Owner: string
        Repo: string
        Number: string
    }

    let getInfoFromUrl (s: string) =
        let regex = Regex("github.com\/(?<owner>\S+)\/(?<repo>\S+)\/pull\/(?<number>\d+)")
        let m = regex.Match s

        {
            Owner = m.Groups.[1].Value
            Repo = m.Groups.[2].Value
            Number = m.Groups.[3].Value
        }


    let getContentsFromGitHub (info: PRInfo) = async {
        let repoInfoUrl =
            sprintf "https://api.github.com/repos/%s/%s/contents/paket.lock" info.Owner info.Repo

        let! (repoInfo: Contents) =

            Fetch.get (repoInfoUrl) |> Async.AwaitPromise

        return repoInfo
    }

    let getPRFilesFromGitHub (info: PRInfo) = async {
        let repoInfoUrl =
            sprintf "https://api.github.com/repos/%s/%s/pulls/%s/files" info.Owner info.Repo info.Number

        let! (repoInfo: PRFiles list) = Fetch.get (repoInfoUrl) |> Async.AwaitPromise

        return repoInfo
    }

    let fetchOldAndNewer (prURL: string) = async {
        let info = getInfoFromUrl prURL
        let! contents = getContentsFromGitHub info
        let! prFiles = getPRFilesFromGitHub info

        let newerLockFile = prFiles |> List.find (fun f -> f.filename = "paket.lock")

        let! (newerContents: Contents) = Fetch.get (newerLockFile.contents_url) |> Async.AwaitPromise

        return contents.download_url, newerContents.download_url
    }


type Model = {
    InputTypeChoice: InputType
    OutputTypeChoice: OutputType
    OlderLockFile: PaketLockFile
    OlderLockUrl: string
    NewerLockFile: PaketLockFile
    NewerLockUrl: string
    GitHubPRUrl: string
    CompareResults: CompareResults
    VersionInfo: VersionInfo option
}


type Msg =
    | OlderLockChanged of PaketLockFile
    | NewerLockChanged of PaketLockFile
    | OlderLockUrlChanged of string
    | OlderLockUrlFetched of Result<PaketLockFile, exn>
    | NewerLockUrlChanged of string
    | NewerLockUrlFetched of Result<PaketLockFile, exn>
    | GitHubPRFetched of Result<string * string, exn>
    | GitHubPRUrlChanged of string
    | RequestComparison
    | ComparisonFinished of Result<PaketDiff, exn>
    | InputTypeChoiceChanged of InputType
    | OutputTypeChoiceChanged of OutputType
    | VersionInfoFetch
    | VersionInfoFetched of Result<VersionInfo, exn>

let paketLockDiffApi =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<IPaketLockDiffApi>


[<Literal>]
let OlderLockFileUrlQueryParam = "olderLockFileUrl"

[<Literal>]
let NewerLockFileUrlQueryParam = "newerLockFileUrl"

let init () : Model * Cmd<Msg> =
    let queryStringBuilder = URLSearchParams.Create(Dom.window.location.search)

    let olderLockFileUrlCmd =
        queryStringBuilder.get OlderLockFileUrlQueryParam
        |> Option.map OlderLockUrlChanged

    let newerLockFileUrlCmd =
        queryStringBuilder.get NewerLockFileUrlQueryParam
        |> Option.map NewerLockUrlChanged

    let msgs =
        [ olderLockFileUrlCmd; newerLockFileUrlCmd; Some VersionInfoFetch ]
        |> List.choose id

    let cmd = msgs |> List.map Cmd.ofMsg |> Cmd.batch

    let model = {
        InputTypeChoice = InputType.Url
        OutputTypeChoice = OutputType.Rich
        OlderLockFile = ""
        OlderLockUrl = ""
        NewerLockFile = ""
        NewerLockUrl = ""
        GitHubPRUrl = ""
        CompareResults = NotStarted
        VersionInfo = None
    }

    model, cmd

let isNullOrWhitespace (s: string) =
    match s with
    | _ when isNullOrUndefined s -> true
    | null -> true
    | "" -> true
    | _ when s.Trim() = "" -> true
    | _ -> false

let requestDiff (model: Model) =
    if
        not <| isNullOrWhitespace model.OlderLockFile
        && not <| isNullOrWhitespace model.NewerLockFile
    then
        Cmd.ofMsg RequestComparison
    else
        Cmd.none

let update (msg: Msg) (model: Model) : Model * Cmd<Msg> =
    match msg with
    | OlderLockChanged olderLockFile ->
        let model = {
            model with
                OlderLockFile = olderLockFile
        }

        model, requestDiff model
    | NewerLockChanged newerLockFile ->
        let model = {
            model with
                NewerLockFile = newerLockFile
        }

        model, requestDiff model
    | RequestComparison ->
        let compareRequest = PaketLocks.create model.OlderLockFile model.NewerLockFile

        let cmd =
            Cmd.OfAsync.either
                paketLockDiffApi.comparePaketLocks
                compareRequest
                (Ok >> ComparisonFinished)
                (Error >> ComparisonFinished)

        { model with CompareResults = Loading }, cmd

    | ComparisonFinished result ->
        let compareResults =
            match result with
            | Ok r -> Finished r
            | Error e -> Errored e

        {
            model with
                CompareResults = compareResults
        },
        Cmd.none
    | InputTypeChoiceChanged(ty) -> { model with InputTypeChoice = ty }, Cmd.none
    | OutputTypeChoiceChanged(ty) -> { model with OutputTypeChoice = ty }, Cmd.none
    | OlderLockUrlChanged(url) ->
        let model = { model with OlderLockUrl = url }

        let queryStringBuilder = URLSearchParams.Create(Dom.window.location.search)
        queryStringBuilder.set (OlderLockFileUrlQueryParam, url)
        let queryString = sprintf "?%s" (string queryStringBuilder)
        Dom.window.history.replaceState (null, null, queryString)

        let cmd =
            if String.notNullOrEmpty url then
                Cmd.OfAsync.either Fetcher.getFromUrl url (OlderLockUrlFetched) (Error >> OlderLockUrlFetched)
            else
                Cmd.none

        model, cmd
    | OlderLockUrlFetched(paketLockFile) ->
        match paketLockFile with
        | Ok file -> model, OlderLockChanged file |> Cmd.ofMsg
        | Error e ->
            {
                model with
                    CompareResults = Errored e
            },
            Cmd.none
    | NewerLockUrlChanged(url) ->
        let model = { model with NewerLockUrl = url }
        let queryStringBuilder = URLSearchParams.Create(Dom.window.location.search)
        queryStringBuilder.set (NewerLockFileUrlQueryParam, url)
        let queryString = sprintf "?%s" (string queryStringBuilder)
        Dom.window.history.replaceState (null, null, queryString)

        let cmd =
            if String.notNullOrEmpty url then
                Cmd.OfAsync.either Fetcher.getFromUrl url (NewerLockUrlFetched) (Error >> NewerLockUrlFetched)
            else
                Cmd.none

        model, cmd
    | NewerLockUrlFetched(paketLockFile) ->
        match paketLockFile with
        | Ok file -> model, NewerLockChanged file |> Cmd.ofMsg
        | Error e ->
            {
                model with
                    CompareResults = Errored e
            },
            Cmd.none
    | GitHubPRUrlChanged(url) ->
        let model = { model with GitHubPRUrl = url }

        let cmd =
            if String.notNullOrEmpty url then
                Cmd.OfAsync.either GitHub.fetchOldAndNewer url (Ok >> GitHubPRFetched) (Error >> GitHubPRFetched)
            else
                Cmd.none

        model, cmd
    | GitHubPRFetched(result) ->
        match result with
        | Ok(olderUrl, newerUrl) ->
            let cmd =
                Cmd.batch [
                    OlderLockUrlChanged olderUrl |> Cmd.ofMsg
                    NewerLockUrlChanged newerUrl |> Cmd.ofMsg
                ]

            model, cmd
        | Error e ->
            {
                model with
                    CompareResults = Errored e
            },
            Cmd.none
    | VersionInfoFetch ->
        let cmd =
            Cmd.OfAsync.either paketLockDiffApi.versionInfo () (Ok >> VersionInfoFetched) (Error >> VersionInfoFetched)

        model, cmd
    | VersionInfoFetched res ->
        match res with
        | Ok info -> { model with VersionInfo = Some info }, Cmd.none
        | Error e ->
            printfn "%A" e
            model, Cmd.none

open Fable.React
open Fable.React.Props
// open Fulma
open Fable.FontAwesome
open Thoth.Json
open Feliz
open Feliz.Bulma

let navBrand =
    Bulma.navbarStart.div [
        Bulma.navbarItem.a [
            prop.href "https://github.com/TheAngryByrd/paket-lock-diff"
            prop.children [ Fa.span [ Fa.Brand.Github; Fa.PullLeft ] []; span [] [ str " GitHub Repo" ] ]
        ]
        Bulma.navbarItem.a [
            prop.href "https://github.com/fsprojects/Paket"
            prop.children [
                Html.img [
                    prop.src "https://raw.githubusercontent.com/fsprojects/Paket/master/docs/files/img/logo.png"
                    prop.alt "Logo"
                    prop.style [ style.marginRight (length.em 0.3); style.maxHeight (length.em 1.75) ]
                ]
                span [] [ str "Paket" ]
            ]
        ]
        Bulma.navbarItem.a [
            prop.href "https://safe-stack.github.io/"
            prop.children [
                Html.img [
                    prop.src "/favicon.png"
                    prop.alt "Logo"
                    prop.style [ style.marginRight (length.em 0.3); style.maxHeight (length.em 1.75) ]
                ]
                span [] [ str "SAFE Stack" ]
            ]
        ]

    ]



let copyToClipboard element =
    let codeElement = document.querySelector element
    let range = document.createRange ()
    range.selectNode codeElement
    window.getSelection().addRange (range)

    // try
    document.execCommand ("copy") |> ignore

    window.getSelection().removeAllRanges ()
// with
// _ -> ()

let createToClipboardElement elementToCopy =

    Bulma.button.button [
        prop.style [ style.margin (length.em 0.3) ]
        prop.onClick (fun _ -> copyToClipboard elementToCopy)
        prop.text "Copy to Clipboard"
    ]

let fugetLink (packageName: string) (version: string) =
    sprintf "https://www.fuget.org/packages/%s/%s/" packageName version

let fugetDiffLink (packageName: string) (oldVersion: string) (newVersion: string) =
    // using anything for the framework moniker selects the first one. Setting it to "unknown" for now.
    sprintf "https://www.fuget.org/packages/%s/%s/lib/unknown/diff/%s/" packageName newVersion oldVersion


let compareResults (paketDiff: PaketDiff) (model: Model) (dispatch: Msg -> unit) =
    let printPackageRich (xs: Shared.Package list) =
        xs
        |> List.groupBy (fun g -> g.GroupName)
        |> List.collect (fun (groupName, packages) -> [
            p [] [ str <| sprintf "%s - %d" groupName packages.Length ]
            for x in packages do
                p [ Style [ Margin ".3em" ] ] [
                    a [ Href(fugetLink x.PackageName x.Version); Target "_blank" ] [
                        str <| sprintf "\u00A0\u00A0%s - %s" x.PackageName x.Version
                    ]
                ]
        ])

    let markdownPrintPackage (xs: Shared.Package list) =
        xs
        |> List.groupBy (fun g -> g.GroupName)
        |> List.collect (fun (groupName, packages) -> [
            Markdown.li <| sprintf "%s - (%d)" groupName packages.Length
            for x in packages do
                Markdown.lii 2
                <| sprintf "[%s - %s](%s)" x.PackageName x.Version (fugetLink x.PackageName x.Version)
            str <| "\n"
        ])


    let printVersionDiff (xs: Shared.PackageVersionDiff list) =
        xs
        |> List.groupBy (fun g -> g.GroupName)
        |> List.collect (fun (groupName, packages) -> [

            p [] [
                Bulma.field.div [
                    prop.className "is-grouped-multiline"
                    prop.children [
                        span [ Style [ MarginRight ".5em" ] ] [ str <| sprintf "%s - %d" groupName packages.Length ]
                    ]
                ]
                let createTag x length =
                    Bulma.control.div [
                        Bulma.tags [
                            prop.className "has-addons"
                            let color =
                                match x with
                                | Major -> "is-danger" |> Some
                                | Minor -> "is-warning" |> Some
                                | Patch -> "is-info" |> Some
                                | _ -> None

                            match color with
                            | Some c ->
                                prop.children [
                                    Bulma.tag [
                                        prop.className c

                                        prop.text (sprintf "%A" x)
                                    ]

                                    Bulma.tag [ prop.className "is-light"; prop.text (sprintf "%d" length) ]
                                ]

                            | None -> ()
                        ]
                    ]

                for (group, ps) in packages |> List.groupBy (fun p -> p.SemVerChange) do
                    p [ Style [ Margin ".3em" ] ] [
                        Bulma.field.div [
                            prop.className "is-grouped-multiline"
                            prop.children [ createTag group ps.Length ]

                        ]
                    ]

                    for x in ps do
                        div [ Style [ MarginRight ".5em" ] ] [

                            a [
                                Href(fugetDiffLink x.PackageName x.OlderVersion x.NewerVersion)
                                Target "_blank"
                            ] [
                                str
                                <| sprintf
                                    "\u00A0\u00A0\u00A0\u00A0%s - %s -> %s"
                                    x.PackageName
                                    x.OlderVersion
                                    x.NewerVersion
                            ]
                        ]
            ]

        ])


    let printVersionDiffMarkdown (xs: Shared.PackageVersionDiff list) =
        xs
        |> List.groupBy (fun g -> g.GroupName)
        |> List.collect (fun (groupName, packages) -> [
            Markdown.li <| sprintf "%s - (%d)" groupName packages.Length

            let groupTitle x length =
                match x with
                | Major -> sprintf "Major - (%d)" length
                | Minor -> sprintf "Minor - (%d)" length
                | Patch -> sprintf "Patch - (%d)" length
                | _ -> ""

            for (group, ps) in packages |> List.groupBy (fun p -> p.SemVerChange) do
                Markdown.lii 2 <| groupTitle group ps.Length

                for x in ps do
                    Markdown.lii 4
                    <| sprintf
                        "[%s - %s -> %s](%s)"
                        x.PackageName
                        x.OlderVersion
                        x.NewerVersion
                        (fugetDiffLink x.PackageName x.OlderVersion x.NewerVersion)

            str "\n"
        ])


    Bulma.container [
        Bulma.tabs [
            prop.className "is-fullwidth is-boxed"
            prop.children [
                Bulma.tab [
                    if model.OutputTypeChoice.IsRich2 then
                        prop.className "is-active"
                    prop.children [
                        a [ OnClick(fun ev -> OutputTypeChoiceChanged OutputType.Rich |> dispatch) ] [
                            Fa.i [ Fa.IconOption.Icon "fab fa-html5" ] []
                            span [ Style [ Margin "0 0 0 .5em" ] ] [ str "Rich" ]
                        ]
                    ]
                ]
                Bulma.tab [
                    if model.OutputTypeChoice.IsMarkdown2 then
                        prop.className "is-active"

                    prop.children [
                        a [ OnClick(fun ev -> OutputTypeChoiceChanged OutputType.Markdown |> dispatch) ] [
                            Fa.i [ Fa.IconOption.Icon "fab fa-markdown" ] []
                            span [ Style [ Margin "0 0 0 .5em" ] ] [ str "Markdown" ]
                        ]
                    ]
                ]
                Bulma.tab [
                    if model.OutputTypeChoice.IsJson2 then
                        prop.className "is-active"

                    prop.children [
                        a [ OnClick(fun ev -> OutputTypeChoiceChanged OutputType.Json |> dispatch) ] [
                            span [] [ str "{ }" ]
                            span [ Style [ Margin "0 0 0 .5em" ] ] [ str "Json" ]
                        ]
                    ]
                ]
            ]
        ]
        match model.OutputTypeChoice with
        | Rich ->
            Bulma.box [
                Bulma.title [ str <| sprintf "Additions - %d" paketDiff.Additions.Length ]
                yield! printPackageRich paketDiff.Additions
            ]

            Bulma.box [
                Bulma.title [ str <| sprintf "Removals - %d" paketDiff.Removals.Length ]
                yield! printPackageRich paketDiff.Removals
            ]

            Bulma.box [
                Bulma.title [ str <| sprintf "Version Upgrades - %d" paketDiff.VersionUpgrades.Length ]
                yield! printVersionDiff paketDiff.VersionUpgrades
            ]

            Bulma.box [
                Bulma.title [ str <| sprintf "Version Downgrades - %d" paketDiff.VersionDowngrades.Length ]
                yield! printVersionDiff paketDiff.VersionDowngrades
            ]
        | Markdown ->
            createToClipboardElement "#markdown-output"

            pre [ Id "markdown-output" ] [
                code [] [

                    Markdown.heading1 "Paket Lock Diff Report"

                    str
                    <| sprintf
                        "This report was generated via [Paket Lock Diff](%s)\n\n"
                        (Dom.window.location.ToString())
                    Markdown.heading2 <| sprintf "Additions - (%d)" paketDiff.Additions.Length
                    yield! markdownPrintPackage paketDiff.Additions
                    Markdown.heading2 <| sprintf "Removals - (%d)" paketDiff.Removals.Length
                    yield! markdownPrintPackage paketDiff.Removals
                    Markdown.heading2
                    <| sprintf "Version Upgrades - (%d)" paketDiff.VersionUpgrades.Length
                    yield! printVersionDiffMarkdown paketDiff.VersionUpgrades
                    Markdown.heading2
                    <| sprintf "Version Downgrades - (%d)" paketDiff.VersionDowngrades.Length
                    yield! printVersionDiffMarkdown paketDiff.VersionDowngrades
                ]
            ]
        | Json ->
            createToClipboardElement "#json-output"

            pre [ Id "json-output" ] [ code [] [ str <| Json.pretty (4, paketDiff) ] ]
    ]

let rawTextDiffBoxes (model: Model) (dispatch: Msg -> unit) =
    Bulma.columns [
        Bulma.column [
            prop.className "is-6"
            prop.children [
                Bulma.box [
                    Bulma.field.div [
                        Bulma.label [ prop.text "Older LockFile Text" ]
                        Bulma.control.div [
                            Bulma.textarea [
                                prop.onChange (fun (x: Types.Event) -> OlderLockChanged(x.Value) |> dispatch)
                                prop.value model.OlderLockFile
                            ]
                        ]
                    ]
                ]
            ]
        ]
        Bulma.column [
            prop.className "is-6"
            prop.children [
                Bulma.box [
                    Bulma.field.div [
                        Bulma.label [ prop.text "Newer LockFile Text" ]
                        Bulma.control.div [
                            Bulma.textarea [
                                prop.onChange (fun (x: Types.Event) -> NewerLockChanged(x.Value) |> dispatch)
                                prop.value model.NewerLockFile
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]

let urlDiffBoxes (model: Model) (dispatch: Msg -> unit) =

    Bulma.columns [
        Bulma.column [
            prop.className "is-6"
            prop.children [
                Bulma.box [
                    Bulma.field.div [
                        Bulma.label [ prop.text "Older LockFile Url" ]
                        Bulma.control.div [
                            Bulma.input.text [
                                prop.onChange (fun (x: Types.Event) -> OlderLockUrlChanged(x.Value) |> dispatch)
                                prop.value model.OlderLockUrl
                            ]
                        ]
                    ]
                ]
            ]
        ]
        Bulma.column [
            prop.className "is-6"
            prop.children [
                Bulma.box [
                    Bulma.field.div [
                        Bulma.label [ prop.text "Newer LockFile Url" ]
                        Bulma.control.div [
                            Bulma.input.text [
                                prop.onChange (fun (x: Types.Event) -> NewerLockUrlChanged(x.Value) |> dispatch)
                                prop.value model.NewerLockUrl
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]

let githubPRBoxes (model: Model) (dispatch: Msg -> unit) =

    Bulma.columns [
        Bulma.column [
            prop.className "is-full"
            prop.children [
                Bulma.box [
                    Bulma.field.div [
                        Bulma.label [ prop.text "GitHub Pull Request Url" ]
                        Bulma.control.div [
                            Bulma.input.text [
                                prop.onChange (fun (x: Types.Event) -> GitHubPRUrlChanged(x.Value) |> dispatch)
                                prop.value model.GitHubPRUrl
                            ]
                        ]
                    ]
                ]
            ]
        ]
    ]


let footer (model: Model) dispatch =
    match model.VersionInfo with
    | Some info ->

        Bulma.columns [
            Bulma.column [ p [] [ str $"Paket.Core Version {info.PaketCore}" ] ]
            Bulma.column [ p [] [ str $"paket-lock-diff Version {info.PaketLockDiff}" ] ]
        ]
    | None -> nothing

let view (model: Model) (dispatch: Msg -> unit) =
    div [ Class "flex-wrapper" ] [
        Bulma.navbar [ prop.children [ Bulma.container [ navBrand ] ] ]

        Bulma.section [
            Bulma.container [
                Bulma.column [

                    Bulma.title [ prop.text "Paket Diff Tool"; prop.className "has-text-centered" ]
                    Bulma.tabs [
                        prop.className "is-fullwidth is-boxed"
                        prop.children [
                            Bulma.tab [
                                if model.InputTypeChoice.IsUrl2 then
                                    prop.className "is-active"
                                prop.children [
                                    Html.a [
                                        Fa.i [ Fa.IconOption.Icon "fas fa-link" ] []
                                        span [ Style [ Margin "0 0 0 .5em" ] ] [ str "Url" ]
                                    ]
                                ]
                                prop.onClick (fun _ -> InputTypeChoiceChanged InputType.Url |> dispatch)
                            ]
                            Bulma.tab [
                                if model.InputTypeChoice.IsGitHubPR2 then
                                    prop.className "is-active"
                                prop.children [
                                    Html.a [
                                        Fa.i [ Fa.IconOption.Icon "fab fa-github" ] []
                                        span [ Style [ Margin "0 0 0 .5em" ] ] [ str "GitHub Pull Request" ]
                                    ]
                                ]
                                prop.onClick (fun _ -> InputTypeChoiceChanged InputType.GitHubPR |> dispatch)
                            ]
                            Bulma.tab [
                                if model.InputTypeChoice.IsRawText2 then
                                    prop.className "is-active"
                                prop.children [
                                    Html.a [
                                        Fa.i [ Fa.IconOption.Icon "fas fa-file-alt" ] []
                                        span [ Style [ Margin "0 0 0 .5em" ] ] [ str "Raw Text" ]
                                    ]
                                ]
                                prop.onClick (fun _ -> InputTypeChoiceChanged InputType.RawText |> dispatch)
                            ]
                        ]
                    ]

                    match model.InputTypeChoice with
                    | InputType.RawText -> rawTextDiffBoxes model dispatch
                    | InputType.Url -> urlDiffBoxes model dispatch
                    | InputType.GitHubPR -> githubPRBoxes model dispatch
                ]
            ]
        ]
        Bulma.section [
            match model.CompareResults with
            | Finished m -> compareResults m model dispatch
            | Loading ->
                Bulma.container [
                    Bulma.progress [ prop.className "is-primary is-small" ]

                ]
            | Errored e ->
                match e with
                | :? ProxyRequestException as e ->
                    let er = Decode.Auto.fromString<ErrorResponse<ParseError>> e.ResponseText

                    let errorElems =
                        match er with
                        | Ok er -> [
                            div [ Class "block" ] [ str <| sprintf "%A" er.error.Message ]
                            div [ Class "block" ] [ str <| sprintf "%A" er.error.InnerMessage ]
                            div [ Class "block" ] [ str <| sprintf "%A" er.error.StackTrace ]
                          ]
                        | Error _ -> [ div [ Class "block" ] [ str <| sprintf "%A" e.Message ] ]

                    Bulma.notification [
                        prop.className "is-danger"
                        prop.children [ Bulma.title [ str "Error" ]; yield! errorElems ]
                    ]
                | e ->
                    Bulma.notification [

                        prop.className "is-danger"
                        prop.children [
                            Bulma.title [ str "Error" ]
                            div [ Class "block" ] [ str <| sprintf "%A" e.Message ]
                            div [ Class "block" ] [ str <| sprintf "%A" e.StackTrace ]
                        ]
                    ]
            | NotStarted -> nothing
        ]


        Bulma.footer [ footer model dispatch ]
    ]
