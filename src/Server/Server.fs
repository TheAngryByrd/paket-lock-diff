namespace Server

module App =

    open Giraffe
    open Microsoft.AspNetCore.Builder
    open Microsoft.AspNetCore.Hosting
    open Microsoft.AspNetCore.Http.Features
    open Microsoft.Extensions.DependencyInjection

    let webApp =
        choose [
            GET
            >=> route "/"
            >=> Handlers.index
            POST
            >=> route "/compare"
            >=> Handlers.compare
        ]

    let configureServices (services: IServiceCollection) =
        services.AddGiraffe()
        |> ignore

        services.AddResponseCompression()
        |> ignore

        services.Configure<FormOptions>(fun (options: FormOptions) ->
            options.ValueLengthLimit <-
                16
                * 1024
                * 1024
        )
        |> ignore

    [<EntryPoint>]
    let main args =
        let builder = WebApplication.CreateBuilder args

        builder.WebHost.UseStaticWebAssets()
        |> ignore

        configureServices builder.Services

        let app = builder.Build()

        app.UseResponseCompression()
        |> ignore

        app.UseStaticFiles()
        |> ignore

        app.MapStaticAssets()
        |> ignore

        app.UseGiraffe webApp
        app.Run()
        0
