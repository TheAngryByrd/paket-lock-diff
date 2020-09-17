module Index

open Elmish
open Fable.Core.JsInterop
open Fable.Remoting.Client
open Shared

type PaketLockFile = string
type Model =
    {
        OlderLockFile: PaketLockFile
        NewerLockFile: PaketLockFile
        CompareResults : Shared.PaketDiff option
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
            CompareResults = None
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
        model, cmd
    | ComparisonFinished result ->
        { model with CompareResults = Some result}, Cmd.none

open Fable.React
open Fable.React.Props
open Fulma

let navBrand =
    Navbar.Brand.div [ ] [
        Navbar.Item.a [
            Navbar.Item.Props [ Href "https://safe-stack.github.io/" ]
            Navbar.Item.IsActive true
        ] [
            img [
                Src "/favicon.png"
                Alt "Logo"
            ]
        ]
    ]

let compareResults (model : PaketDiff) (dispatch : Msg -> unit) =
    let printPackage (xs : Shared.Package list) =
        xs
        |> List.groupBy(fun g -> g.GroupName)
        |> List.collect(fun (groupName, packages) ->
            printfn "%A groupname" groupName
            [
                p [ ] [ str groupName ]
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
            printfn "%A groupname" groupName
            [
                p [ ] [ str groupName ]
                for x in packages do
                    p [ ][
                      str <| sprintf "\u00A0\u00A0%s - %s -> %s" x.PackageName x.OlderVersion x.NewerVersion
                    ]
            ]
        )
    Column.column [Column.Width (Screen.All, Column.Is12)] [
        Container.container [] [
            Tile.ancestor [ ]
                [
                    Tile.parent [ Tile.IsVertical
                                  Tile.Size Tile.Is6 ]
                        [ Tile.child [ ] [
                            Box.box' [ ] [
                                Heading.p [ ]
                                    [ str <| sprintf "Additions - %d" model.Additions.Length ]
                                yield! printPackage model.Additions
                            ]

                            ]
                          Tile.child [ ]
                            [ Box.box' [ ]
                                [ Heading.p [ ]
                                    [ str <| sprintf "Removals - %d" model.Removals.Length ]

                                  yield! printPackage model.Removals
                                ]
                            ]
                        ]
                    Tile.parent [   Tile.IsVertical
                                    Tile.Size Tile.Is6 ]
                        [ Tile.child [ ]
                            [ Box.box' [ ]
                                [ Heading.p [ ]
                                    [ str <| sprintf "Version Increases - %d" model.VersionIncreases.Length]
                                  yield! printVersionDiff model.VersionIncreases
                                ] ]
                          Tile.child [ ]
                            [ Box.box' [ ]
                                [ Heading.p [ ]
                                    [ str <| sprintf "Version Decreases - %d" model.VersionDecreases.Length ]
                                  yield! printVersionDiff model.VersionDecreases
                                ] ]
                        ]
                ]
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
        //         Navbar.navbar [ ] [
        //             Container.container [ ] [ navBrand ]
        //         ]
        //     ]


        // ]
        Section.section [ ] [
                Container.container [ ] [
                    Column.column [
                        // Column.Width (Screen.All, Column.Is6)
                        // Column.Offset (Screen.All, Column.Is3)
                    ] [
                        Heading.p [ Heading.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ] [ str "Paket Diff Tool" ]
                        diffBoxes model dispatch
                    ]
                ]

            ]
        Section.section [ ] [
            match model.CompareResults with
            | Some m ->
                compareResults m dispatch
            | None -> ()
        ]
    ]
