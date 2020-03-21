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

type Model =
  | NewGame of ServerNewGame
  | Running of ServerRunningGame
  | Finished of FinishedGame
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

let shuffle cards = cards |> List.sortBy (fun _ -> Guid.NewGuid())
  
let singleDeckOfCards =
  CardRank.allValues |> List.collect (fun r -> CardSuit.allValues |> List.map (fun s -> (s,r)))

let deckOfCards =
  [ 1..4 ] 
  |> List.collect (fun _ -> singleDeckOfCards)
  |> List.map (fun v -> { Id = Guid.NewGuid(); Value = v; Position = FaceDown; Selected = false; })

let deal players = 
  let shuffledCards = shuffle deckOfCards
  let playersWithNewCards, undealtCards =
    players
    |> List.fold 
      (fun (playersDealt, accumulatedCards : Card list) player -> 
          let playerRow1Cards, playerRow2Cards, remainingCards = accumulatedCards.[..5], accumulatedCards.[6..11], accumulatedCards.[12..]
          let playerCards = { Row1 = playerRow1Cards; Row2 = playerRow2Cards; Hand = None; }
          ({ player with Cards = playerCards; FinalScore = None; } :: playersDealt, remainingCards)
      ) 
      (List.empty<Player>,shuffledCards)

  let discardPile, drawPile, serverDrawPile = [{ undealtCards.Head with Position = FaceUp }], undealtCards.Tail.[..6], undealtCards.Tail.[7..]

  let sortedPlayersWithNewCards = playersWithNewCards |> List.sortBy (fun x -> x.Id)
  { UndrawnCards = serverDrawPile; Game = { DrawPile = drawPile ; DiscardPile = discardPile; Players = sortedPlayersWithNewCards; MovingCard = None; CurrentPlayer = sortedPlayersWithNewCards |> List.tryHead; }}

let getCardPlayArea model card =
  match model.CurrentPlayer with
  | None -> None
  | Some player ->
    model.DrawPile
    |> List.tryFindIndex (fun x -> x = card)
    |> Option.map (fun i -> PlayArea.DrawPile)
    |> Option.orElseWith (fun _ ->
      model.DiscardPile 
      |> List.tryFindIndex (fun x -> x = card)
      |> Option.map (fun i -> PlayArea.DiscardPile)  
      |> Option.orElseWith (fun _ ->
        player.Cards.Row1 
        |> List.tryFindIndex (fun x -> x = card)
        |> Option.map (fun i -> PlayArea.Row1)          
      )     
      |> Option.orElseWith (fun _ ->
        player.Cards.Row2 
        |> List.tryFindIndex (fun x -> x = card)
        |> Option.map (fun i -> PlayArea.Row2)          
      )     
      |> Option.orElseWith (fun _ ->
        player.Cards.Hand
        |> Option.bind (fun x -> if x = card then Some PlayArea.Hand else None)
      ) 
    )

// let flipAndSelect card =
//   let newPosition = 
//     match card.Position with
//     | FaceUp -> FaceDown
//     | FaceDown -> FaceUp
//   { card with Position = newPosition; Selected = true; }

let flipUp card = { card with Position = FaceUp; }
let flipDown card = { card with Position = FaceDown; }

let scoreCards cards =
  List.zip cards.Row1 cards.Row2
  |> List.map (fun (r1,r2) -> 
    let firstScore =
      match r1.Value, r2.Value with
      | (_,Joker),(_,Joker) -> -10 + (Joker.toInt()) + (Joker.toInt())
      | (_,a),(_,b) when a = b -> 0
      | (_,a),(_,b) -> (a.toInt()) + (b.toInt())
      
    (r1,r2,firstScore)
  )
  |> List.pairwise
  |> List.sumBy (fun ((a,b,c),(d,e,f)) -> 
    let matchingColumnScore = if (a,b) = (d,e) then -10 else 0
    matchingColumnScore + c + f
  )

let scorePlayer player =
  let score = scoreCards player.Cards
  { player with FinalScore = Some score; Score = player.Score + score }

let removeCard cards card = cards |> List.filter (fun c -> c <> card)
let replaceCard cards newCard oldCard = cards |> List.map (fun c -> if c = oldCard then newCard else c) 
// let tryFlip cards card = cards |> List.map (fun c -> if c = card then flip c else c )
let tryFlipUp cards card = cards |> List.map (fun c -> if c = card then flipUp c else c )

let blankGame = (NewGame { Players = [] })
let addPlayer (p : NewPlayer) = { Player.Id = p.Id; ClientId = p.ClientId; Name = p.Name; Cards = { Row1 = []; Row2 = []; Hand = None;}; FinalScore = None; Score = 0; }
let endGame (players : Player list) replyChannel =
  let finishedGame = { FinishedGame.Players = players |> List.map scorePlayer }
  let newModel = Finished finishedGame
  replyChannel (finishedGame |> ClientMsg.FinishedGame |> BroadcastReply)
  newModel
  
let endTurn (game : RunningGame) =
  let newGameState =
      let playersUpdatedFromPreviousTurn = game.CurrentPlayer |> Option.map (fun player -> game.Players |> List.map (fun x -> if x.Id = player.Id then player else x) |> List.sortBy (fun x -> x.Id)) |> Option.defaultValue game.Players
      let newCurrentPlayer = game.CurrentPlayer |> Option.bind (fun player -> game.Players |> List.sortBy (fun x -> x.Id) |> List.tryFind (fun x -> x.Id > player.Id)) |> Option.orElse (game.Players |> List.tryHead)
      { game with Players = playersUpdatedFromPreviousTurn; CurrentPlayer = newCurrentPlayer; }
      
  newGameState
  
type GameTracker() =
  static let agent = MailboxProcessor.Start(fun inbox ->
    let rec messageLoop currentModel = async{
      let! (msg : ServerMsg), replyChannel = inbox.Receive()
      let newState =
        try
          match currentModel, msg with
          | _, ServerMsg.RunningGameMsg AbortGame ->
            replyChannel (ClientMsg.RunningGameMsg AbortedGame |> BroadcastReply)
            blankGame
          | NewGame model, ServerMsg.RecoverGame sharedGameState when model.Players.Length = 0 ->
            let newModel =
              match sharedGameState with
              | SharedGameState.NewGame sharedNewGame -> NewGame { ServerNewGame.Players = sharedNewGame.Players }
              | SharedGameState.Finished sharedFinishedGame -> Finished sharedFinishedGame
              | SharedGameState.Running sharedRunningGame -> 
                let shuffledCards = shuffle deckOfCards
                let undrawnCards = shuffledCards  // |> List.filter (fun x -> sharedRunningGame.DiscardPile |> List.exists (fun d -> d.Value = x.Value)) // TODO remove cards from player's hands when recovering draw pile
                Running { ServerRunningGame.UndrawnCards = undrawnCards; Game = sharedRunningGame }
            newModel
          | NewGame model, ServerMsg.NewGameMsg (AddPlayer newPlayer) ->
            let newModel = { model with Players = newPlayer :: model.Players }
            printfn "%A" newModel
            // if it is a new client, send them everyone
            if model.Players |> List.exists (fun x -> x.ClientId = newPlayer.ClientId)
            then
              replyChannel (PlayerAdded newPlayer |> ClientMsg.NewGameMsg |> BroadcastReply)
            else
              replyChannel (SyncPlayers newModel.Players |> ClientMsg.NewGameMsg |> BroadcastReply)
            NewGame newModel
          | NewGame model, ServerMsg.NewGameMsg GetExistingPlayers ->
            replyChannel (SyncPlayers model.Players |> ClientMsg.NewGameMsg |> BroadcastReply)
            currentModel
          | NewGame model, ServerMsg.NewGameMsg (SetPlayerStatus (playerId, newStatus)) ->
            let updatedPlayers = model.Players |> List.map (fun x -> if x.Id = playerId then { x with Status = newStatus} else x)
            if updatedPlayers |> List.forall (fun x -> x.Status = NewPlayerStatus.Ready) |> not then
              let newModel = { model with Players = updatedPlayers }
              replyChannel (PlayerStatusUpdated (playerId, newStatus) |> ClientMsg.NewGameMsg |> BroadcastReply)
              NewGame newModel
            else
              let runningGame = updatedPlayers |> List.map addPlayer |> deal
              replyChannel (DealtGame runningGame.Game |> ClientMsg.NewGameMsg |> BroadcastReply)
              Running runningGame
          | NewGame model, ServerMsg.NewGameMsg Deal when model.Players.Length > 0 ->
            let runningGame = model.Players |> List.map addPlayer |> deal
            replyChannel (DealtGame runningGame.Game |> ClientMsg.NewGameMsg |> BroadcastReply)
            Running runningGame
          | NewGame model , ServerMsg.RunningGameMsg RequestState ->
            replyChannel (AbortedGame |> ClientMsg.RunningGameMsg |> BroadcastReply)
            currentModel
          | Running model, ServerMsg.RunningGameMsg msg ->
            let isGameOngoing = model.Game.Players |> List.exists (fun x -> x.Cards.Row1 |> List.exists (fun c -> c.Position = FaceDown) || x.Cards.Row2 |> List.exists (fun c -> c.Position = FaceDown))
            match model.Game.CurrentPlayer, msg with
            | _ when (not isGameOngoing) ->
              endGame model.Game.Players replyChannel
            | _ , RequestState ->
              replyChannel (SharedGameState.Running model.Game |> ClientMsg.GameState |> BroadcastReply)
              currentModel
            | None, _ -> 
              endGame model.Game.Players replyChannel
            | _, EndTurn ->
              let newGameState = endTurn model.Game
              replyChannel (SharedGameState.Running newGameState |> ClientMsg.GameState |> BroadcastReply)
              Running { model with Game = newGameState }
            | Some player, FlipCard card ->
              let newRunningState = 
                // Don't flip if card in hand
                match player.Cards.Hand.IsSome with
                | true -> model
                | false ->
                  getCardPlayArea model.Game card
                  |> Option.map (fun playArea -> 
                    match playArea with
                    | PlayArea.DrawPile ->
                      // Don't flip draw pile if any card in draw pile already flipped
                      if model.Game.DrawPile |> List.exists (fun c -> c.Position = FaceUp) then model
                      else { model with Game = { model.Game with DrawPile = tryFlipUp model.Game.DrawPile card }}
                    | PlayArea.Row1 ->
                      { model with Game = { model.Game with CurrentPlayer = Some { player with Cards = { player.Cards with Row1 = tryFlipUp player.Cards.Row1 card; } } } }
                    | PlayArea.Row2 ->
                      { model with Game = { model.Game with CurrentPlayer = Some { player with Cards = { player.Cards with Row2 = tryFlipUp player.Cards.Row2 card; } } } }
                    // Don't flip cards in hand or discard pile
                    | PlayArea.Hand
                    | PlayArea.DiscardPile -> model
                  )
                  |> Option.defaultValue model
              replyChannel (SharedGameState.Running newRunningState.Game |> ClientMsg.GameState |> BroadcastReply)
              Running newRunningState
            | _, Move selectedCard ->
              let newRunningState = { model with Game = { model.Game with MovingCard = Some selectedCard }}
              replyChannel (SharedGameState.Running newRunningState.Game |> ClientMsg.GameState |> BroadcastReply)
              Running newRunningState
            | _, CancelMove ->
              let newRunningState = {model with Game = { model.Game with MovingCard = None} }
              replyChannel (SharedGameState.Running newRunningState.Game |> ClientMsg.GameState |> BroadcastReply)
              Running newRunningState
            | Some player, DropOn dropDestination ->
              let newRunningState, shouldEndTurn =
                model.Game.MovingCard
                |> Option.map (fun sourceCard -> 
                  getCardPlayArea model.Game sourceCard
                  |> Option.map (fun (sourceArea) ->
                    match sourceArea, dropDestination with
                    | PlayArea.DrawPile, DiscardPile ->
                      let replacementCard, serverDrawPile = model.UndrawnCards.Head, model.UndrawnCards.Tail
                      let game =  
                        { model.Game with
                            DrawPile = replaceCard model.Game.DrawPile replacementCard sourceCard
                            DiscardPile = sourceCard :: (model.Game.DiscardPile |> List.map flipDown)
                            MovingCard = None }
                      { model with UndrawnCards = serverDrawPile; Game = game }, true
                    | PlayArea.DrawPile, Row1 destinationCard ->
                      let replacementCard, serverDrawPile = model.UndrawnCards.Head, model.UndrawnCards.Tail
                      let game =  
                        { model.Game with
                            CurrentPlayer = Some { player with Cards = { player.Cards with Row1 = replaceCard player.Cards.Row1 sourceCard destinationCard; Hand = Some (flipUp destinationCard); } }
                            DrawPile = replaceCard model.Game.DrawPile replacementCard sourceCard
                            MovingCard = None }
                      { model with UndrawnCards = serverDrawPile; Game = game }, false
                    | PlayArea.DrawPile, Row2 destinationCard ->
                      let replacementCard, serverDrawPile = model.UndrawnCards.Head, model.UndrawnCards.Tail
                      let game =  
                        { model.Game with 
                            CurrentPlayer = Some { player with Cards = { player.Cards with Row2 = replaceCard player.Cards.Row2 sourceCard destinationCard; Hand = Some (flipUp destinationCard); } }
                            DrawPile = replaceCard model.Game.DrawPile replacementCard sourceCard
                            MovingCard = None }
                      { model with UndrawnCards = serverDrawPile; Game = game }, false
                    | PlayArea.Hand, DiscardPile ->
                      let game =  
                        { model.Game with
                            CurrentPlayer = Some { player with Cards = { player.Cards with Hand = None; } }
                            DiscardPile = sourceCard :: (model.Game.DiscardPile |> List.map flipDown)
                            MovingCard = None }
                      { model with Game = game }, true
                    | PlayArea.DiscardPile, Row1 destinationCard ->
                      let game =  
                        { model.Game with
                            CurrentPlayer = Some { player with Cards = { player.Cards with Row1 = replaceCard player.Cards.Row1 sourceCard destinationCard; Hand = Some (flipUp destinationCard); } }
                            DiscardPile = model.Game.DiscardPile.Tail |> List.mapi (fun i x -> if i = 0 then flipUp x else x)
                            MovingCard = None }
                      { model with Game = game }, false
                    | PlayArea.DiscardPile, Row2 destinationCard ->
                      let game =  
                        { model.Game with
                            CurrentPlayer = Some { player with Cards = { player.Cards with Row2 = replaceCard player.Cards.Row2 sourceCard destinationCard; Hand = Some (flipUp destinationCard); } }
                            DiscardPile = model.Game.DiscardPile.Tail |> List.mapi (fun i x -> if i = 0 then flipUp x else x)
                            MovingCard = None }
                      { model with Game = game }, false
                    | PlayArea.Hand, Row1 destinationCard ->
                      let game =  
                        { model.Game with
                            CurrentPlayer = Some { player with Cards = { player.Cards with Row1 = replaceCard player.Cards.Row1 sourceCard destinationCard; Hand = Some (flipUp destinationCard); } }
                            MovingCard = None }
                      { model with Game = game }, false
                    | PlayArea.Hand, Row2 destinationCard ->
                      let game =  
                        { model.Game with
                            CurrentPlayer = Some { player with Cards = { player.Cards with Row2 = replaceCard player.Cards.Row2 sourceCard destinationCard; Hand = Some (flipUp destinationCard); } }
                            MovingCard = None }
                      { model with Game = game }, false
                    | _ -> model, false
                  )
                  |> Option.defaultValue (model, false)
                )
                |> Option.defaultValue (model, false)

              let finalGameState =
                if shouldEndTurn then
                  { newRunningState with Game = endTurn newRunningState.Game }
                else 
                  newRunningState
              replyChannel (SharedGameState.Running finalGameState.Game |> ClientMsg.GameState |> BroadcastReply)
              Running finalGameState
            | _, GameOver ->
              let finishedGame = { FinishedGame.Players = model.Game.Players |> List.map scorePlayer }
              let newModel = Finished finishedGame
              replyChannel (finishedGame |> ClientMsg.FinishedGame |> BroadcastReply)
              newModel
            | _ -> Running model
          | Finished model, ServerMsg.NewGameMsg Deal ->
            let runningGame = deal model.Players
            replyChannel (DealtGame runningGame.Game |> ClientMsg.NewGameMsg |> BroadcastReply)
            Running runningGame
          | NewGame _, (ServerMsg.RunningGameMsg _)
          | Running _, (ServerMsg.NewGameMsg _)
          | Finished _, (ServerMsg.NewGameMsg _)
          | Finished _, (ServerMsg.RunningGameMsg _)
          | _ ->
            printfn "Unhandled msg: %A" msg
            currentModel

        with
        | ex -> // This is best effort. If it fails, oh well.
          //replyChannel [||]
          currentModel
      return! messageLoop newState
      }
    messageLoop blankGame
    )
  static member Post msg = agent.PostAndReply (fun replyChannel -> msg, replyChannel.Reply)

/// Sets up the channel to listen to clients.
let channel = channel {
    join (fun ctx socketId ->
        task {
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
