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
    with
        member x.IsRawText2 = match x with | RawText _ -> true | _ -> false
        member x.IsUrl2 = match x with | Url _ -> true | _ -> false

module Fetcher =
    let getFromUrl (url) = async {
        let! result = fetch url [] |> Async.AwaitPromise
        if result.Ok then
            let! body = result.text() |> Async.AwaitPromise
            return Ok body
        else
            let! body = result.text() |> Async.AwaitPromise
            return Error (Exception (body))
    }

type Model =
    {
        InputTypeChoice : InputType
        OlderLockFile: PaketLockFile
        OlderLockUrl: string
        NewerLockFile: PaketLockFile
        NewerLockUrl: string
        CompareResults : CompareResults
    }


type Msg =
    | OlderLockChanged of PaketLockFile
    | NewerLockChanged of PaketLockFile
    | OlderLockUrlChanged of string
    | OlderLockUrlFetched of Result<PaketLockFile,exn>
    | NewerLockUrlChanged of string
    | NewerLockUrlFetched of Result<PaketLockFile,exn>
    | RequestComparison
    | ComparisonFinished of Result<PaketDiff,exn>
    | InputTypeChoiceChanged of InputType

let paketLockDiffApi =
    Remoting.createApi()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<IPaketLockDiffApi>

let [<Literal>] OlderLockFileUrlQueryParam = "olderLockFileUrl"
let [<Literal>] NewerLockFileUrlQueryParam = "newerLockFileUrl"

let init(): Model * Cmd<Msg> =
    let queryStringBuilder = URLSearchParams.Create(Dom.window.location.search)
    let olderLockFileUrlCmd =
        queryStringBuilder.get OlderLockFileUrlQueryParam
        |> Option.map OlderLockUrlChanged
    let newerLockFileUrlCmd =
        queryStringBuilder.get NewerLockFileUrlQueryParam
        |> Option.map NewerLockUrlChanged
    let msgs =
        [
            olderLockFileUrlCmd
            newerLockFileUrlCmd
        ]
        |> List.choose id
    let cmd =
        msgs
        |> List.map Cmd.ofMsg
        |> Cmd.batch
    let model =
        {
            InputTypeChoice = InputType.Url
            OlderLockFile = ""
            OlderLockUrl = ""
            NewerLockFile = ""
            NewerLockUrl = ""
            CompareResults = NotStarted
        }
    model, cmd

let isNullOrWhitespace (s : string) =
    match s with
    | _ when isNullOrUndefined s -> true
    | null -> true
    | "" -> true
    | _ when s.Trim() = "" -> true
    | _ -> false

let requestDiff (model : Model) =
    if not <| isNullOrWhitespace model.OlderLockFile && not <| isNullOrWhitespace model.NewerLockFile then
        Cmd.ofMsg RequestComparison
    else
        Cmd.none

let update (msg: Msg) (model: Model): Model * Cmd<Msg> =
    match msg with
    | OlderLockChanged olderLockFile ->
        let model = { model with OlderLockFile = olderLockFile }
        model, requestDiff model
    | NewerLockChanged newerLockFile ->
        let model = { model with NewerLockFile = newerLockFile }
        model, requestDiff model
    | RequestComparison ->
        let compareRequest = PaketLocks.create model.OlderLockFile model.NewerLockFile
        let cmd = Cmd.OfAsync.either paketLockDiffApi.comparePaketLocks compareRequest (Ok >> ComparisonFinished) (Error >> ComparisonFinished)

        { model with CompareResults = Loading }, cmd

    | ComparisonFinished result ->
        let compareResults =
            match result with
            | Ok r -> Finished r
            | Error e -> Errored e
        { model with CompareResults = compareResults }, Cmd.none
    | InputTypeChoiceChanged(ty) ->
        { model with InputTypeChoice = ty }, Cmd.none
    | OlderLockUrlChanged(url) ->
        let model = {model with OlderLockUrl = url}

        let queryStringBuilder = URLSearchParams.Create(Dom.window.location.search)
        queryStringBuilder.append(OlderLockFileUrlQueryParam, url)
        let queryString = sprintf "?%s" (string queryStringBuilder)
        Dom.window.history.replaceState(null,null, queryString)
        let cmd =
            if String.notNullOrEmpty url then
                Cmd.OfAsync.either Fetcher.getFromUrl url (OlderLockUrlFetched) (Error >> OlderLockUrlFetched)
            else
                Cmd.none
        model, cmd
    | OlderLockUrlFetched(paketLockFile) ->
        match paketLockFile with
        | Ok file ->
            model, OlderLockChanged file |> Cmd.ofMsg
        | Error e ->
            printfn "%A" e
            model, Cmd.none
    | NewerLockUrlChanged(url) ->
        let model = {model with NewerLockUrl = url}
        let queryStringBuilder = URLSearchParams.Create(Dom.window.location.search)
        queryStringBuilder.append(NewerLockFileUrlQueryParam, url)
        let queryString = sprintf "?%s" (string queryStringBuilder)
        Dom.window.history.replaceState(null,null, queryString)
        let cmd =
            if String.notNullOrEmpty url then
                Cmd.OfAsync.either Fetcher.getFromUrl url (NewerLockUrlFetched) (Error >> NewerLockUrlFetched)
            else
                Cmd.none
        model, cmd
    | NewerLockUrlFetched(paketLockFile) ->
        match paketLockFile with
        | Ok file ->
            model, NewerLockChanged file |> Cmd.ofMsg
        | Error e ->
            printfn "%A" e
            model, Cmd.none

open Fable.React
open Fable.React.Props
open Fulma
open Fable.FontAwesome
open Thoth.Json

let navBrand =
    Navbar.Brand.div [ ] [

        Navbar.Item.a [
            Navbar.Item.Props [ Href "https://github.com/TheAngryByrd/paket-lock-diff" ]
            Navbar.Item.IsActive false
        ] [
            Fa.span [ Fa.Brand.Github; Fa.PullLeft ][ ]
            span [] [
                str " GitHub Repo"
            ]
        ]
        Navbar.Item.a [
            Navbar.Item.Props [ Href "https://github.com/fsprojects/Paket" ]
            Navbar.Item.IsActive false
        ] [
            img [
                Src "https://raw.githubusercontent.com/fsprojects/Paket/master/docs/files/img/logo.png"
                Alt "Logo"
                Style [MarginRight ".3em"]
            ]
            span [] [
                str "Paket"
            ]
        ]
        Navbar.Item.a [
            Navbar.Item.Props [ Href "https://safe-stack.github.io/" ]
            Navbar.Item.IsActive false
        ] [
            img [
                Src "/favicon.png"
                Alt "Logo"
                Style [MarginRight ".3em"]
            ]
            span [] [
                str "SAFE Stack"
            ]
        ]
    ]

let compareResults (model : PaketDiff) (dispatch : Msg -> unit) =
    let printPackage (xs : Shared.Package list) =
        xs
        |> List.groupBy(fun g -> g.GroupName)
        |> List.collect(fun (groupName, packages) ->
            [
                p [  ] [
                    str <| sprintf "%s - %d" groupName packages.Length
                ]
                for x in packages do
                    p [ Style [Margin ".3em"] ][
                        str <| sprintf "\u00A0\u00A0%s - %s" x.PackageName x.Version
                    ]
            ]
        )
    let printVersionDiff (xs : Shared.PackageVersionDiff list) =
        xs
        |> List.groupBy(fun g -> g.GroupName)
        |> List.collect(fun (groupName, packages) ->
            [
                let majorLength = packages |> List.filter(fun p -> match p.SemVerChange with | Major -> true | _ -> false) |> List.length
                let minorLength = packages |> List.filter(fun p -> match p.SemVerChange with | Minor -> true | _ -> false) |> List.length
                let patchLength = packages |> List.filter(fun p -> match p.SemVerChange with | Patch -> true | _ -> false) |> List.length
                p [ ] [
                    Field.div [ Field.IsGroupedMultiline ] [
                        span [Style [MarginRight ".5em"] ] [
                            str <| sprintf "%s - %d" groupName packages.Length
                        ]
                        Control.div [] [
                            Tag.list [ Tag.List.HasAddons ] [
                                Tag.tag [Tag.Color IsDanger] [
                                    str "Major"
                                ]
                                Tag.tag [Tag.Color IsLight ] [
                                    str <| sprintf "%d" majorLength
                                ]
                            ]
                        ]
                        Control.div [] [
                            Tag.list [ Tag.List.HasAddons ] [
                                Tag.tag [Tag.Color IsWarning] [
                                    str "Minor"
                                ]
                                Tag.tag [Tag.Color IsLight ] [
                                    str <| sprintf "%d" minorLength
                                ]
                            ]
                        ]
                        Control.div [] [
                            Tag.list [ Tag.List.HasAddons ] [
                                Tag.tag [Tag.Color IsInfo] [
                                    str "Patch"
                                ]
                                Tag.tag [Tag.Color IsLight ] [
                                    str <| sprintf "%d" patchLength
                                ]
                            ]
                        ]
                    ]

                ]
                for x in packages do
                    p [ Style [Margin ".3em"]][
                        Field.div [ Field.IsGroupedMultiline ] [
                            span [Style [MarginRight ".5em"] ] [
                                str <| sprintf "\u00A0\u00A0%s - %s -> %s" x.PackageName x.OlderVersion x.NewerVersion
                            ]
                            Control.div [] [
                                Tag.list [ Tag.List.HasAddons ] [
                                    let color =
                                        match x.SemVerChange with
                                        | Major -> Tag.Color IsDanger |> Some
                                        | Minor -> Tag.Color IsWarning |> Some
                                        | Patch -> Tag.Color IsInfo |> Some
                                        | _ -> None
                                    match color with
                                    | Some c ->
                                        Tag.tag [c] [
                                            str <| sprintf "%A" x.SemVerChange
                                        ]
                                    | None -> ()
                                ]
                            ]
                        ]
                    ]
            ]
        )
    Container.container [] [
        Box.box' [] [
            Heading.p [ ] [
                str <| sprintf "Additions - %d" model.Additions.Length
            ]
            yield! printPackage model.Additions
        ]
        Box.box' [] [
            Heading.p [ ] [
                str <| sprintf "Removals - %d" model.Removals.Length
            ]
            yield! printPackage model.Removals
        ]
        Box.box' [] [
            Heading.p [ ] [
                str <| sprintf "Version Upgrades - %d" model.VersionUpgrades.Length
            ]
            yield! printVersionDiff model.VersionUpgrades
        ]
        Box.box' [] [
            Heading.p [ ] [
                str <| sprintf "Version Downgrades - %d" model.VersionDowngrades.Length
            ]
            yield! printVersionDiff model.VersionDowngrades
        ]
    ]

let rawTextDiffBoxes (model : Model) (dispatch : Msg -> unit) =
    Columns.columns [] [
        Column.column [Column.Width (Screen.All, Column.Is6)] [
            Box.box' [] [
                Field.div [ ]  [
                    Label.label [ ] [ str "Older LockFile Text" ]
                    Control.div [ ] [
                        Textarea.textarea [ Textarea.OnChange (fun x -> OlderLockChanged (x.Value) |> dispatch) ] [ str model.OlderLockFile ]
                    ]
                ]
            ]
        ]
        Column.column [Column.Width (Screen.All, Column.Is6)] [
            Box.box' [] [
                Field.div [ ][
                    Label.label [ ] [ str "Newer LockFile Text" ]
                    Control.div [ ] [
                        Textarea.textarea [ Textarea.OnChange (fun x -> NewerLockChanged (x.Value) |> dispatch) ] [ str model.NewerLockFile ]
                    ]
                ]
            ]
        ]
    ]

let urlDiffBoxes (model : Model) (dispatch : Msg -> unit) =
    Columns.columns [] [
        Column.column [Column.Width (Screen.All, Column.Is6)] [
            Box.box' [] [
                Field.div [ ]  [
                    Label.label [ ] [ str "Older LockFile Url" ]
                    Control.div [ ] [
                        Input.text [ Input.Option.Value model.OlderLockUrl; Input.Option.OnChange (fun x -> OlderLockUrlChanged (x.Value) |> dispatch) ]
                    ]
                ]
            ]
        ]
        Column.column [Column.Width (Screen.All, Column.Is6)] [
            Box.box' [] [
                Field.div [ ][
                    Label.label [ ] [ str "Newer LockFile Url" ]
                    Control.div [ ] [
                        Input.text [ Input.Option.Value model.NewerLockUrl;  Input.Option.OnChange (fun x -> NewerLockUrlChanged (x.Value) |> dispatch) ]
                        ]
                ]
            ]
        ]
    ]

let errorBox elems =
    Container.container [] [
        Notification.notification [ Notification.Color IsDanger] [
                h1 [Class "title"] [
                    str "Error"
                ]
                yield! elems
        ]
    ]

let view (model : Model) (dispatch : Msg -> unit) =
    div [] [
        Navbar.navbar [ ] [
            Container.container [ ] [ navBrand ]
        ]

        Section.section [ ] [
            Container.container [ ] [
                Column.column [] [
                    Heading.p [ Heading.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ] [ str "Paket Diff Tool" ]
                    Tabs.tabs [ Tabs.IsFullWidth; Tabs.IsBoxed] [
                        Tabs.tab [Tabs.Tab.IsActive model.InputTypeChoice.IsUrl2] [
                            a [OnClick (fun ev -> InputTypeChoiceChanged InputType.Url |> dispatch)] [
                                Fa.i [Fa.IconOption.Icon "fas fa-link"] []
                                span [Style [Margin "0 0 0 .5em"]] [str "Url"]
                            ]
                        ]
                        Tabs.tab [Tabs.Tab.IsActive model.InputTypeChoice.IsRawText2] [

                            a [OnClick (fun ev -> InputTypeChoiceChanged InputType.RawText |> dispatch)] [
                                Fa.i [Fa.IconOption.Icon "fas fa-file-alt"] []
                                span [Style [Margin "0 0 0 .5em"]] [str "Raw Text"]
                            ]
                        ]
                    ]
                    match model.InputTypeChoice with
                    | InputType.RawText ->
                        rawTextDiffBoxes model dispatch
                    | InputType.Url ->
                        urlDiffBoxes model dispatch
                ]
            ]
        ]
        Section.section [ ] [
            match model.CompareResults with
            | Finished m ->
                compareResults m dispatch
            | Loading ->
                Container.container [ ] [
                    Progress.progress [Progress.Color Color.IsPrimary; Progress.Size Size.IsSmall] []
                ]
            | Errored e ->
                match e with
                | :? ProxyRequestException as e ->
                    let er = Decode.Auto.fromString<ErrorResponse<ParseError>> e.ResponseText
                    let errorElems =
                        match er with
                        | Ok er ->
                            [
                                div [Class "block"] [
                                    str <| sprintf "%A" er.error.Message
                                ]
                                div [Class "block"] [
                                    str <| sprintf "%A" er.error.InnerMessage
                                ]
                                div [Class "block"] [
                                    str <| sprintf "%A" er.error.StackTrace
                                ]
                            ]
                        | Error _ ->
                            [
                                div [Class "block"] [
                                    str <| sprintf "%A" e.Message
                                ]
                            ]
                    errorBox errorElems
                | _ -> ()
            | NotStarted ->
                ()
        ]
    ]
