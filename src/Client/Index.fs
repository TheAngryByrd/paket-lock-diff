module Index

open Elmish
open Fable.Core.JsInterop
open Fable.Remoting.Client
open Shared

type PaketLockFile = string

type CompareResults =
| Finished of Shared.PaketDiff
| Loading
| NotStarted

type Model =
    {
        OlderLockFile: PaketLockFile
        NewerLockFile: PaketLockFile
        CompareResults : CompareResults
    }


type Msg =
    | OlderLockChanged of PaketLockFile
    | NewerLockChanged of PaketLockFile
    | RequestComparison
    | ComparisonFinished of PaketDiff

let todosApi =
    Remoting.createApi()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.buildProxy<ITodosApi>

let init(): Model * Cmd<Msg> =
    let model =
        {
            OlderLockFile = ""
            NewerLockFile = ""
            CompareResults = NotStarted
        }
    model, Cmd.none

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
        let cmd = Cmd.OfAsync.perform todosApi.comparePaketLocks compareRequest ComparisonFinished
        { model with CompareResults = Loading }, cmd
    | ComparisonFinished result ->
        { model with CompareResults = Finished result}, Cmd.none

open Fable.React
open Fable.React.Props
open Fulma
open Fable.FontAwesome

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
                p [ ] [ str <| sprintf "%s - %d" groupName packages.Length  ]
                for x in packages do
                    p [ ][
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
                p [ ] [ str <| sprintf "%s - %d -- major(%d) minor(%d) patch(%d)" groupName packages.Length majorLength minorLength patchLength ]
                for x in packages do

                    p [ ][

                      str <| sprintf "\u00A0\u00A0%s - %s -> %s - (%A)" x.PackageName x.OlderVersion x.NewerVersion x.SemVerChange
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


let diffBoxes (model : Model) (dispatch : Msg -> unit) =
    Columns.columns [] [
        Column.column [Column.Width (Screen.All, Column.Is6)] [
            Box.box' [] [
                Field.div [ ]
                    [ Label.label [ ]
                        [ str "Older LockFile" ]
                      Control.div [ ]
                        [ Textarea.textarea [ Textarea.OnChange (fun x -> OlderLockChanged x.Value |> dispatch) ]
                            [ ] ] ]
            ]
        ]
        Column.column [Column.Width (Screen.All, Column.Is6)] [
            Box.box' [] [
                Field.div [ ]
                    [ Label.label [ ]
                        [ str "Newer Lock File" ]
                      Control.div [ ]
                        [ Textarea.textarea [ Textarea.OnChange (fun x -> NewerLockChanged x.Value |> dispatch) ]
                            [ ] ] ]
            ]
        ]
    ]

let view (model : Model) (dispatch : Msg -> unit) =
    div [] [
        Navbar.navbar [ ] [
                Container.container [ ] [ navBrand ]
            ]
        // Hero.hero [
        //     // Hero.Color IsPrimary
        //     Hero.IsFullHeight
        //     Hero.Props [
        //         Style [
        //             Background """linear-gradient(rgba(0, 0, 0, 0.5), rgba(0, 0, 0, 0.5)), url("https://unsplash.it/1200/900?random") no-repeat center center fixed"""
        //             BackgroundSize "cover"
        //         ]
        //     ]
        // ] [
        //     Hero.head [ ] [

        //     ]


        // ]
        Section.section [ ] [
                Container.container [ ] [
                    Column.column [
                    ] [
                        Heading.p [ Heading.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ] [ str "Paket Diff Tool" ]
                        diffBoxes model dispatch
                    ]
                ]

            ]
        Section.section [ ] [
            match model.CompareResults with
            | Finished m ->
                compareResults m dispatch
            | Loading ->
                Container.container [ ] [
                    Progress.progress [Progress.Color Color.IsPrimary; Progress.Size Size.IsSmall] [

                    ]
                ]
            | NotStarted ->
                ()
        ]
    ]
