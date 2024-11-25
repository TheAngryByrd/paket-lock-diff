module App

open Elmish
open Elmish.React

open Fable.Core.JsInterop


#if DEBUG
open Elmish.HMR
#endif

Program.mkProgram Index.init Index.update Index.view
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactSynchronous "elmish-app"

|> Program.run