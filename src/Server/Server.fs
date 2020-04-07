open System.IO
open System.Threading.Tasks

open Microsoft.AspNetCore.Builder
open Microsoft.Extensions.DependencyInjection
open FSharp.Control.Tasks.V2
open Giraffe
open Saturn
open Shared
open System
open Thoth.Json.Net

let tryGetEnv = System.Environment.GetEnvironmentVariable >> function null | "" -> None | x -> Some x

//let publicPath = Path.GetFullPath "../Client/public"

let publicPath =
  tryGetEnv "public_path"
  |> Option.defaultValue "./public"
  |> Path.GetFullPath

let port =
    "SERVER_PORT"
    |> tryGetEnv |> Option.map uint16 |> Option.defaultValue 8085us

type ServerNewGame = { Players : NewPlayer list; }

type Model = GameEngingModel

type AgentReplyType =
  | BroadcastReply of ClientMsg

/// Sends a message to a specific client by their socket ID.
let sendMessage (hub:Channels.ISocketHub) socketId (payload:ClientMsg) = task {
    let payload = Encode.Auto.toString(0, payload)
    do! hub.SendMessageToClient "/channel" socketId "" payload
}

/// Sends a message to all connected clients.
let broadcastMessage (hub:Channels.ISocketHub) (payload:ClientMsg) = task {
    let payload = Encode.Auto.toString(0, payload)
    do! hub.SendMessageToClients "/channel" "" payload
}

// let flipAndSelect card =
//   let newPosition = 
//     match card.Position with
//     | FaceUp -> FaceDown
//     | FaceDown -> FaceUp
//   { card with Position = newPosition; Selected = true; }

type GameTracker() =
  static let agent = MailboxProcessor.Start(fun inbox ->
    let rec messageLoop (currentModel : GameState) = async{
      let! (msg : ServerMsg), replyChannel = inbox.Receive()
      let newState, newMsgs = Golf.handleUpdate currentModel msg
      printfn "sending client msgs %A" newMsgs
      newMsgs
      |> List.iter (BroadcastReply >> replyChannel)
      return! messageLoop newState
      }
    messageLoop Golf.blankGame
    )
  static member Post msg = agent.PostAndReply (fun replyChannel -> msg, replyChannel.Reply)

/// Sets up the channel to listen to clients.
let channel = channel {
    join (fun ctx socketId ->
        task {
            printfn "Client has connected. They've been assigned socket Id: %A" socketId
            //ctx.GetLogger().LogInformation("Client has connected. They've been assigned socket Id: {socketId}", socketId)
            return Channels.Ok
        })
    handle "" (fun ctx message ->
        task {
            let hub = ctx.GetService<Channels.ISocketHub>()
            let msg = message.Payload |> string |> Decode.Auto.unsafeFromString<ServerMsg>

            // Here we handle any websocket client messages in a type-safe manner
        
            let replyChannelReply = GameTracker.Post msg
            let x =
                match replyChannelReply with
                | BroadcastReply m -> 
                  broadcastMessage hub m
            do! x
        })
}

open Giraffe.GiraffeViewEngine

let golfLayout = 
  let index =
    [ yield div [ _id "elmish-app" ] []
      yield script [ _src (HashInfo.withHash "/resources/vendors" "js") ] []
      yield script [ _src (HashInfo.withHash "/resources/golfApp" "js") ] [] ]
  Layout.layout index

let tensLayout = 
  let index =
    [ yield div [ _id "elmish-app" ] []
      yield script [ _src (HashInfo.withHash "/resources/vendors" "js") ] []
      yield script [ _src (HashInfo.withHash "/resources/tensApp" "js") ] [] ]
  Layout.layout index

let defaultView =
  router {
    get "/" (htmlView Index.layout)
    //get "/" (redirectTo false "/app")
    get "/index.html" (redirectTo false "/app")
    get "/default.html" (redirectTo false "/app")
    get "/empty" (text "Hello")
    // pipe_through authorize auth not required to access app. However auth is required for API routes.
    get "/app/golf" (htmlView golfLayout)
    get "/app/tens" (htmlView tensLayout)
    get "/app" (htmlView App.layout)
  }

let browser =
  pipeline {
    plug acceptHtml
    // plug acceptJson
    // plug putSecureBrowserHeaders
    plug fetchSession
    set_header "x-pipeline-type" "Browser"
  }

let browserRouter =
  router {
    // not_found_handler (text "Browser 404")
    // not_found_handler (htmlView NotFound.layout) //Use the default 404 webpage
    pipe_through browser //Use the default browser pipeline
    forward "" defaultView //Use the default view
  }

let api = pipeline {
            // plug acceptJson
            // plug acceptHtml
            set_header "x-pipeline-type" "Api" }

// let authorize =
//   requiresAuthentication (challenge Microsoft.AspNetCore.Authentication.AzureAD.UI.AzureADDefaults.JwtBearerAuthenticationScheme)

let abortGame (next: HttpFunc) (ctx: Microsoft.AspNetCore.Http.HttpContext) =
    task {
      let hub = ctx.GetService<Channels.ISocketHub>()

      let replyChannelReply = GameTracker.Post (ServerMsg.RunningGameMsg AbortGame)
      let x =
          match replyChannelReply with
          | BroadcastReply m -> 
            broadcastMessage hub m
      do! x
      return! next ctx
    }

let apiRouter =
  router {
    pipe_through api
    //pipe_through authorize
    get "/AbortGame" abortGame
  }

let appRouter = router {
    //forward "/account" accountRouter
    not_found_handler (setStatusCode 404 >=> text "Not Found")
    forward "" browserRouter
    forward "/api" apiRouter
  }

let webApp = 
  choose [
      appRouter
  ]

let app = application {
    url ("http://0.0.0.0:" + port.ToString() + "/")
    use_router webApp
    memory_cache
    use_static publicPath
    use_json_serializer(Thoth.Json.Giraffe.ThothSerializer())
    use_gzip
    add_channel "/channel" channel
    disable_diagnostics
}

run app
