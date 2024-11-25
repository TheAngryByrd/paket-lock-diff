namespace Server

module App =

    open SAFE
    open Saturn
    open Shared
    open Server.Core

    open Microsoft.AspNetCore.Http
    open Microsoft.Extensions.DependencyInjection

    open Fable.Remoting.Server
    open Fable.Remoting.Giraffe
    open Saturn
    open Shared
    open System

    let api (httpContext: HttpContext) = {
        comparePaketLocks =
            fun paketFiles -> async {
                let olderSplit =
                    paketFiles.OlderLockFile
                    |> String.splitByNewlines

                let newerSplit =
                    paketFiles.NewerLockFile
                    |> String.splitByNewlines

                let! comparison = PaketComparer.compare (olderSplit, newerSplit)

                return
                    comparison
                    |> PaketComparer.diffToDTO
            }
        versionInfo = fun () -> async {
            let references = System.Reflection.Assembly.GetExecutingAssembly().GetReferencedAssemblies();
            let paketCore = references |> Array.find(fun a -> a.Name.Contains("Paket.Core"))

            let appVersion = System.Reflection.Assembly.GetExecutingAssembly().GetName().Version.ToString()
            return {
                PaketCore = paketCore.Version.ToString()
                PaketLockDiff = appVersion
            }
        }
    }


    let errorHandler (ex: Exception) (routeInfo: RouteInfo<HttpContext>) =
        printfn "%A" ex

        match ex with
        | ex when ex.Message.Contains("Error during parsing") ->
            let error = {
                Message = ex.Message
                InnerMessage = ex.InnerException.Message
                StackTrace = ex.StackTrace
            }

            Propagate error
        | ex -> Propagate ex.Message

    let webApp =
        Remoting.createApi ()
        |> Remoting.withRouteBuilder Route.builder
        |> Remoting.withErrorHandler errorHandler
        |> Remoting.fromContext api
        |> Remoting.buildHttpHandler

    let configureServices (services: IServiceCollection) = services

    let app = application {
        use_router webApp
        memory_cache
        use_static "public"
        use_gzip
        service_config configureServices
    }

    [<EntryPoint>]
    let main _ =
        run app
        0