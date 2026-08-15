namespace Server

module Views =

    open System
    open System.Text
    open System.Text.Json
    open Giraffe.Htmx
    open Giraffe.ViewEngine
    open Giraffe.ViewEngine.Htmx
    open Shared

    [<RequireQualifiedAccess>]
    type InputType =
        | Url
        | GitHubPullRequest
        | RawText

    type InputUrls = {
        OlderLockFileUrl: string
        NewerLockFileUrl: string
        GitHubPullRequestUrl: string
    }

    module InputType =
        let parse value =
            match value with
            | "github" -> InputType.GitHubPullRequest
            | "raw" -> InputType.RawText
            | _ -> InputType.Url

        let slug =
            function
            | InputType.Url -> "url"
            | InputType.GitHubPullRequest -> "github"
            | InputType.RawText -> "raw"

        let title =
            function
            | InputType.Url -> "URL"
            | InputType.GitHubPullRequest -> "GitHub Pull Request"
            | InputType.RawText -> "Raw Text"

    let private fugetLink (packageName: string) (version: string) =
        $"https://www.fuget.org/packages/{packageName}/{version}/"

    let private fugetDiffLink (packageName: string) (oldVersion: string) (newVersion: string) =
        $"https://www.fuget.org/packages/{packageName}/{newVersion}/lib/unknown/diff/{oldVersion}/"

    let private navBar =
        nav [ _class "navbar" ] [
            div [ _class "container" ] [
                div [ _class "navbar-start" ] [
                    a [
                        _class "navbar-item"
                        _href "https://github.com/TheAngryByrd/paket-lock-diff"
                        _target "_blank"
                        _rel "noreferrer"
                    ] [
                        span [ _class "icon" ] [ i [ _class "fa-brands fa-github" ] [] ]
                        span [] [ str "GitHub Repo" ]
                    ]
                    a [
                        _class "navbar-item"
                        _href "https://github.com/fsprojects/Paket"
                        _target "_blank"
                        _rel "noreferrer"
                    ] [
                        span [ _class "icon" ] [ i [ _class "fa-solid fa-box-open" ] [] ]
                        span [] [ str "Paket" ]
                    ]
                    a [
                        _class "navbar-item"
                        _href "https://giraffe.wiki/"
                        _target "_blank"
                        _rel "noreferrer"
                    ] [
                        span [ _class "icon" ] [ i [ _class "fa-solid fa-server" ] [] ]
                        span [] [ str "Giraffe" ]
                    ]
                ]
            ]
        ]

    let private inputUrl inputType inputUrls =
        let slug = InputType.slug inputType

        [
            "input", slug
            "olderLockFileUrl", inputUrls.OlderLockFileUrl
            "newerLockFileUrl", inputUrls.NewerLockFileUrl
            "githubPullRequestUrl", inputUrls.GitHubPullRequestUrl
        ]
        |> List.choose (fun (name, value) ->
            if String.IsNullOrWhiteSpace value then
                None
            else
                Some $"{name}={Uri.EscapeDataString value}"
        )
        |> String.concat "&"
        |> sprintf "/?%s"

    let private inputTab selected inputType iconClass inputUrls =
        let slug = InputType.slug inputType
        let url = inputUrl inputType inputUrls

        li [
            if selected = inputType then
                _class "is-active"
        ] [
            a [
                _href url
                attr "data-input-tab" slug
            ] [
                span [ _class "icon is-small" ] [ i [ _class iconClass ] [] ]
                span [] [ str (InputType.title inputType) ]
            ]
        ]

    let private compareFormAttributes formId trigger = [
        _id formId
        _method "post"
        _action "/compare"
        _hxPost "/compare"
        _hxTarget "#comparison-results"
        _hxSwap HxSwap.InnerHtml
        _hxTrigger trigger
        _hxIndicator "#comparison-progress"
        _hxSync "#input-section" HxSync.Replace
    ]

    let private compareButton label =
        div [ _class "field" ] [
            div [ _class "control" ] [
                button [
                    _class "button is-primary"
                    _type "submit"
                ] [ str label ]
            ]
        ]

    let private hiddenLockFields = [
        textarea [
            _name "olderLockFile"
            attr "hidden" "hidden"
        ] []
        textarea [
            _name "newerLockFile"
            attr "hidden" "hidden"
        ] []
    ]

    let private urlForm olderUrl newerUrl =
        form
            (attr "data-fetch-mode" "urls"
             :: compareFormAttributes "url-compare-form" "paket-locks-loaded")
            [
                div [ _class "columns" ] [
                    div [ _class "column is-6" ] [
                        div [ _class "box" ] [
                            div [ _class "field" ] [
                                label [
                                    _class "label"
                                    _for "older-lock-url"
                                ] [ str "Older LockFile URL" ]
                                div [ _class "control" ] [
                                    input [
                                        _id "older-lock-url"
                                        _class "input"
                                        _type "url"
                                        _name "olderLockFileUrl"
                                        _value olderUrl
                                        _placeholder "https://example.com/older/paket.lock"
                                        attr "required" "required"
                                    ]
                                ]
                            ]
                        ]
                    ]
                    div [ _class "column is-6" ] [
                        div [ _class "box" ] [
                            div [ _class "field" ] [
                                label [
                                    _class "label"
                                    _for "newer-lock-url"
                                ] [ str "Newer LockFile URL" ]
                                div [ _class "control" ] [
                                    input [
                                        _id "newer-lock-url"
                                        _class "input"
                                        _type "url"
                                        _name "newerLockFileUrl"
                                        _value newerUrl
                                        _placeholder "https://example.com/newer/paket.lock"
                                        attr "required" "required"
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]
                yield! hiddenLockFields
                compareButton "Fetch and compare"
            ]

    let private githubForm githubUrl =
        form
            (attr "data-fetch-mode" "github"
             :: compareFormAttributes "github-compare-form" "paket-locks-loaded")
            [
                div [ _class "box" ] [
                    div [ _class "field" ] [
                        label [
                            _class "label"
                            _for "github-pr-url"
                        ] [ str "GitHub Pull Request URL" ]
                        div [ _class "control" ] [
                            input [
                                _id "github-pr-url"
                                _class "input"
                                _type "url"
                                _name "githubPullRequestUrl"
                                _value githubUrl
                                _placeholder "https://github.com/owner/repository/pull/123"
                                attr "required" "required"
                            ]
                        ]
                    ]
                ]
                yield! hiddenLockFields
                compareButton "Fetch pull request and compare"
            ]

    let private rawTextForm =
        form (compareFormAttributes "raw-compare-form" "submit") [
            div [ _class "columns" ] [
                div [ _class "column is-6" ] [
                    div [ _class "box" ] [
                        div [ _class "field" ] [
                            label [
                                _class "label"
                                _for "older-lock-text"
                            ] [ str "Older LockFile Text" ]
                            div [ _class "control" ] [
                                textarea [
                                    _id "older-lock-text"
                                    _class "textarea lock-input"
                                    _name "olderLockFile"
                                    attr "required" "required"
                                ] []
                            ]
                        ]
                    ]
                ]
                div [ _class "column is-6" ] [
                    div [ _class "box" ] [
                        div [ _class "field" ] [
                            label [
                                _class "label"
                                _for "newer-lock-text"
                            ] [ str "Newer LockFile Text" ]
                            div [ _class "control" ] [
                                textarea [
                                    _id "newer-lock-text"
                                    _class "textarea lock-input"
                                    _name "newerLockFile"
                                    attr "required" "required"
                                ] []
                            ]
                        ]
                    ]
                ]
            ]
            compareButton "Compare"
        ]

    let private inputPanel selected inputType content =
        div
            [
                attr "data-input-panel" (InputType.slug inputType)

                if
                    selected
                    <> inputType
                then
                    attr "hidden" "hidden"
            ]
            content

    let inputSection selected inputUrls =
        section [
            _id "input-section"
            _class "section"
        ] [
            div [ _class "container" ] [
                div [ _class "column" ] [
                    h1 [ _class "title has-text-centered" ] [ str "Paket Diff Tool" ]
                    div [ _class "tabs is-fullwidth is-boxed" ] [
                        ul [] [
                            inputTab selected InputType.Url "fa-solid fa-link" inputUrls
                            inputTab
                                selected
                                InputType.GitHubPullRequest
                                "fa-brands fa-github"
                                inputUrls
                            inputTab selected InputType.RawText "fa-solid fa-file-lines" inputUrls
                        ]
                    ]
                    inputPanel selected InputType.Url [
                        urlForm inputUrls.OlderLockFileUrl inputUrls.NewerLockFileUrl
                    ]
                    inputPanel selected InputType.GitHubPullRequest [
                        githubForm inputUrls.GitHubPullRequestUrl
                    ]
                    inputPanel selected InputType.RawText [ rawTextForm ]
                    progress [
                        _id "comparison-progress"
                        _class "progress is-primary is-small htmx-indicator"
                        _max "100"
                    ] []
                ]
            ]
        ]

    let private packageGroups (packages: Package list) =
        packages
        |> List.groupBy _.GroupName
        |> List.collect (fun (groupName, groupPackages) -> [
            p [] [ strong [] [ str $"{groupName} - {groupPackages.Length}" ] ]
            for package in groupPackages do
                p [ _class "package-line" ] [
                    a [
                        _href (fugetLink package.PackageName package.Version)
                        _target "_blank"
                        _rel "noreferrer"
                    ] [ str $"{package.PackageName} - {package.Version}" ]
                ]
        ])

    let private semVerTag semVerChange count =
        let color =
            match semVerChange with
            | SemVerChange.Major -> "is-danger"
            | SemVerChange.Minor -> "is-warning"
            | SemVerChange.Patch -> "is-info"
            | SemVerChange.Other -> "is-light"

        div [ _class "tags has-addons semver-tags" ] [
            span [ _class $"tag {color}" ] [ str (string semVerChange) ]
            span [ _class "tag is-light" ] [ str (string count) ]
        ]

    let private versionDiffGroups (packages: PackageVersionDiff list) =
        packages
        |> List.groupBy _.GroupName
        |> List.collect (fun (groupName, groupPackages) -> [
            p [] [ strong [] [ str $"{groupName} - {groupPackages.Length}" ] ]
            for semVerChange, changedPackages in
                groupPackages
                |> List.groupBy _.SemVerChange do
                semVerTag semVerChange changedPackages.Length

                for package in changedPackages do
                    p [ _class "package-line nested" ] [
                        a [
                            _href (
                                fugetDiffLink
                                    package.PackageName
                                    package.OlderVersion
                                    package.NewerVersion
                            )
                            _target "_blank"
                            _rel "noreferrer"
                        ] [
                            str
                                $"{package.PackageName} - {package.OlderVersion} -> {package.NewerVersion}"
                        ]
                    ]
        ])

    let private richOutput (diff: PaketDiff) =
        div [ attr "data-output-panel" "rich" ] [
            div [ _class "box" ] [
                h2 [ _class "title is-4" ] [ str $"Additions - {diff.Additions.Length}" ]
                yield! packageGroups diff.Additions
            ]
            div [ _class "box" ] [
                h2 [ _class "title is-4" ] [ str $"Removals - {diff.Removals.Length}" ]
                yield! packageGroups diff.Removals
            ]
            div [ _class "box" ] [
                h2 [ _class "title is-4" ] [
                    str $"Version Upgrades - {diff.VersionUpgrades.Length}"
                ]
                yield! versionDiffGroups diff.VersionUpgrades
            ]
            div [ _class "box" ] [
                h2 [ _class "title is-4" ] [
                    str $"Version Downgrades - {diff.VersionDowngrades.Length}"
                ]
                yield! versionDiffGroups diff.VersionDowngrades
            ]
        ]

    let private appendPackageMarkdown (builder: StringBuilder) title (packages: Package list) =
        builder.AppendLine($"## {title} - ({packages.Length})").AppendLine()
        |> ignore

        for groupName, groupPackages in
            packages
            |> List.groupBy _.GroupName do
            builder.AppendLine($"* {groupName} - ({groupPackages.Length})")
            |> ignore

            for package in groupPackages do
                builder.AppendLine(
                    $"  * [{package.PackageName} - {package.Version}]({fugetLink package.PackageName package.Version})"
                )
                |> ignore

            builder.AppendLine()
            |> ignore

    let private appendVersionMarkdown
        (builder: StringBuilder)
        title
        (packages: PackageVersionDiff list)
        =
        builder.AppendLine($"## {title} - ({packages.Length})").AppendLine()
        |> ignore

        for groupName, groupPackages in
            packages
            |> List.groupBy _.GroupName do
            builder.AppendLine($"* {groupName} - ({groupPackages.Length})")
            |> ignore

            for semVerChange, changedPackages in
                groupPackages
                |> List.groupBy _.SemVerChange do
                builder.AppendLine($"  * {semVerChange} - ({changedPackages.Length})")
                |> ignore

                for package in changedPackages do
                    let link =
                        fugetDiffLink package.PackageName package.OlderVersion package.NewerVersion

                    builder.AppendLine(
                        $"    * [{package.PackageName} - {package.OlderVersion} -> {package.NewerVersion}]({link})"
                    )
                    |> ignore

            builder.AppendLine()
            |> ignore

    let markdown reportUrl (diff: PaketDiff) =
        let builder = StringBuilder()

        builder.AppendLine("# Paket Lock Diff Report").AppendLine()
        |> ignore

        builder
            .AppendLine($"This report was generated via [Paket Lock Diff]({reportUrl})")
            .AppendLine()
        |> ignore

        appendPackageMarkdown builder "Additions" diff.Additions
        appendPackageMarkdown builder "Removals" diff.Removals
        appendVersionMarkdown builder "Version Upgrades" diff.VersionUpgrades
        appendVersionMarkdown builder "Version Downgrades" diff.VersionDowngrades
        builder.ToString()

    let json (diff: PaketDiff) =
        let versionDiffs packages =
            packages
            |> List.map (fun package -> {|
                GroupName = package.GroupName
                PackageName = package.PackageName
                OlderVersion = package.OlderVersion
                NewerVersion = package.NewerVersion
                SemVerChange = string package.SemVerChange
            |})

        let value = {|
            Additions = diff.Additions
            Removals = diff.Removals
            VersionUpgrades = versionDiffs diff.VersionUpgrades
            VersionDowngrades = versionDiffs diff.VersionDowngrades
        |}

        JsonSerializer.Serialize(value, JsonSerializerOptions(WriteIndented = true))

    let private outputTab name label icon isActive =
        li [
            if isActive then
                _class "is-active"
        ] [
            a [
                _href "#"
                attr "data-output-tab" name
            ] [
                if not (String.IsNullOrEmpty icon) then
                    span [ _class "icon is-small" ] [ i [ _class icon ] [] ]

                span [] [ str label ]
            ]
        ]

    let private copyButton target =
        button [
            _class "button copy-button"
            _type "button"
            attr "data-copy-target" target
        ] [
            span [ _class "icon" ] [ i [ _class "fa-solid fa-copy" ] [] ]
            span [] [ str "Copy to Clipboard" ]
        ]

    let results reportUrl diff =
        div [
            _class "container"
            attr "data-output-root" "true"
        ] [
            div [ _class "tabs is-fullwidth is-boxed" ] [
                ul [] [
                    outputTab "rich" "Rich" "fa-brands fa-html5" true
                    outputTab "markdown" "Markdown" "fa-brands fa-markdown" false
                    outputTab "json" "JSON" "" false
                ]
            ]
            richOutput diff
            div [
                attr "data-output-panel" "markdown"
                attr "hidden" "hidden"
            ] [
                copyButton "#markdown-output"
                pre [ _id "markdown-output" ] [ code [] [ str (markdown reportUrl diff) ] ]
            ]
            div [
                attr "data-output-panel" "json"
                attr "hidden" "hidden"
            ] [
                copyButton "#json-output"
                pre [ _id "json-output" ] [ code [] [ str (json diff) ] ]
            ]
        ]

    let validationError message =
        div [
            _class "notification is-warning"
            attr "role" "alert"
        ] [
            h2 [ _class "title is-5" ] [ str "Both lock files are required" ]
            p [] [ str message ]
        ]

    let comparisonError (error: exn) =
        div [
            _class "notification is-danger"
            attr "role" "alert"
        ] [
            h2 [ _class "title is-5" ] [ str "Unable to compare these lock files" ]
            p [] [ str error.Message ]
            match error.InnerException with
            | null -> ()
            | inner -> p [ _class "mt-3" ] [ str inner.Message ]
        ]

    let private footerView versionInfo =
        footer [ _class "footer" ] [
            div [ _class "content has-text-centered" ] [
                p [] [
                    str $"Paket.Core {versionInfo.PaketCore}"
                    str " · "
                    str $"paket-lock-diff {versionInfo.PaketLockDiff}"
                ]
            ]
        ]

    let page inputType inputUrls versionInfo =
        html [ _lang "en" ] [
            head [] [
                meta [ _charset "utf-8" ]
                meta [
                    _name "viewport"
                    _content "width=device-width, initial-scale=1"
                ]
                title [] [ str "Paket Lock Diff Tool" ]
                link [
                    _rel "shortcut icon"
                    _type "image/png"
                    _href
                        "https://raw.githubusercontent.com/fsprojects/Paket/master/docs/files/img/logo.png"
                ]
                link [
                    _rel "stylesheet"
                    _href
                        "https://cdn.jsdelivr.net/npm/@fortawesome/fontawesome-free@7.3.1/css/all.min.css"
                ]
                link [
                    _rel "stylesheet"
                    _href "https://cdn.jsdelivr.net/npm/bulma@1.0.4/css/bulma.min.css"
                ]
                link [
                    _rel "stylesheet"
                    _href "/style.css"
                ]
                Htmx.Script.local
                script [
                    _type "module"
                    _src "/output/App.js"
                ] []
            ]
            body [] [
                div [ _class "flex-wrapper" ] [
                    div [] [
                        navBar
                        inputSection inputType inputUrls
                        section [ _class "section" ] [ div [ _id "comparison-results" ] [] ]
                    ]
                    footerView versionInfo
                ]
            ]
        ]
