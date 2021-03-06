module Client

open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Fetch.Types
open Thoth.Fetch
open Fulma
open Thoth.Json
open System
open Shared
open Fable.Core.JsInterop
open Fable.FontAwesome
// open Thoth.Elmish
open Fable.Core
open Fable.Import
open Browser.Types
open Browser.WebSocket

// Cards from
// https://www.sketchappsources.com/free-source/3060-cards-deck-template-sketch-freebie-resource.html

/// Status of the websocket.
type WsSender = ServerMsg -> unit

let inline decode<'a> x = x |> unbox<string> |> Thoth.Json.Decode.Auto.unsafeFromString<'a>
let buildWsSender (ws:WebSocket) : WsSender =
    fun (message:ServerMsg) ->
        let message = {| Topic = ""; Ref = ""; Payload = message |}
        let message = Thoth.Json.Encode.Auto.toString(0, message)
        ws.send message

type ConnectionState =
    | DisconnectedFromServer | ConnectedToServer of WsSender | Connecting
    member this.IsConnected =
        match this with
        | ConnectedToServer _ -> true
        | DisconnectedFromServer | Connecting -> false

type Model =
  { ClientId : Guid
    GameState : GameState
    ConnectionState : ConnectionState }

type Msg =
  | JoinGame
  | OutgoingMessage of ServerMsg
  | UpdateUserName of string
  | IncomingMsg of ClientMsg
  | ConnectionChange of ConnectionState

let subscription _ =
    let sub dispatch =
        /// Handles push messages from the server and relays them into Elmish messages.
        let onWebSocketMessage (msg:MessageEvent) =
            let msg = msg.data |> decode<{| Payload : string |}>
            msg.Payload
            |> decode<ClientMsg>
            |> IncomingMsg
            |> dispatch
        
        let websocketUrl =
          let loc = Browser.Dom.window.location
          let host = if loc.hostname = "localhost" then "localhost:8085" else loc.host
          let protocol = if loc.protocol = "https:" then "wss:" else "ws:"
          sprintf "%s//%s/channel" protocol host

        /// Continually tries to connect to the server websocket.
        let rec connect () =
            let ws = WebSocket.Create websocketUrl
            ws.onmessage <- onWebSocketMessage
            ws.onopen <- (fun ev ->
                dispatch (ConnectionChange (ConnectedToServer (buildWsSender ws)))
                printfn "WebSocket opened")
            ws.onclose <- (fun ev ->
                dispatch (ConnectionChange DisconnectedFromServer)
                printfn "WebSocket closed. Retrying connection"
                promise {
                    do! Promise.sleep 2000
                    dispatch (ConnectionChange Connecting)
                    connect() })

        connect()

    Cmd.ofSub sub

let pro gameState =
  promise {
      do! Promise.sleep 20000
      return OutgoingMessage (RecoverGame gameState) }

let init () : Model * Cmd<Msg> =
  let gameStateModel = NewGame { NewGame.Players = []; NewUserName = ""; }
  let model =
    { ClientId = Guid.NewGuid()
      ConnectionState = DisconnectedFromServer
      GameState = gameStateModel }
  model, Cmd.none
  
let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
  match msg with
  | ConnectionChange status ->
    let newModel = { currentModel with ConnectionState = status }
    let cmd =
      match status with
      | ConnectedToServer _ -> Cmd.none//Cmd.OfPromise.result (pro currentModel.GameState) //Cmd.ofMsg (OutgoingMessage (RecoverGame currentModel.GameState))
      | DisconnectedFromServer _ -> Cmd.none
      | Connecting _ -> Cmd.none
    newModel, cmd
  | IncomingMsg (ClientMsg.RunningGameMsg AbortedGame) ->
    { currentModel with GameState = Golf.blankGame }, Cmd.none //Cmd.ofMsg (OutgoingMessage (RecoverGame currentModel.GameState))
  | OutgoingMessage om ->
    match currentModel.ConnectionState with
    | ConnectedToServer sender -> sender om
    | _ -> 
      printfn "Unable to send message. Not connected."
      ()
    currentModel, Cmd.none
  | _ ->
    let gameState, cmd =
      match currentModel.GameState, msg with
      | _, IncomingMsg (ClientMsg.GameState gameState) ->
        gameState, Cmd.none
      | _, IncomingMsg (ClientMsg.DealtGame runningGame) ->
        Running (ClientType runningGame), Cmd.none
      | NewGame model, JoinGame -> 
        let newPlayerName = if String.IsNullOrWhiteSpace model.NewUserName then "Player" else model.NewUserName
        let newPlayer = { ClientId = currentModel.ClientId; Id = Guid.NewGuid(); Name = newPlayerName; Status = NewPlayerStatus.Joined }
        NewGame { model with NewUserName = ""; }, Cmd.ofMsg (AddPlayer newPlayer |> ServerMsg.NewGameMsg |> OutgoingMessage)
      | NewGame model, IncomingMsg (ClientMsg.NewGameMsg clientMsg) ->
        match clientMsg with
        | PlayerAdded p ->
          NewGame { model with Players = p :: model.Players; }, Cmd.none
        | SyncPlayers p ->
          NewGame { model with Players = p; }, Cmd.none
        | PlayerStatusUpdated (id, newStatus) ->
          NewGame { model with Players = model.Players |> List.map (fun x -> if x.Id = id then { x with Status = newStatus} else x)}, Cmd.none
      | NewGame model, UpdateUserName v ->
        NewGame { model with NewUserName = v; }, Cmd.none
      | Running model, IncomingMsg (ClientMsg.FinishedGame finishedGame) ->
        Finished finishedGame, Cmd.none
      | NewGame _, IncomingMsg (ClientMsg.RunningGameMsg _)
      | NewGame _, IncomingMsg (ClientMsg.FinishedGame _)
      | Running _, IncomingMsg (ClientMsg.NewGameMsg _)
      | Running _, JoinGame
      | Running _, UpdateUserName _
      | Finished _, JoinGame
      | Finished _, UpdateUserName _
      | Finished _, IncomingMsg (ClientMsg.NewGameMsg _)
      | Finished _, IncomingMsg (ClientMsg.RunningGameMsg _)
      | Finished _, IncomingMsg (ClientMsg.FinishedGame _) 
      | _ ->
        currentModel.GameState, Cmd.none
    { currentModel with GameState = gameState }, cmd

let (++) = List.append

let getTinyCardProps cardOption =
  let flipClassName =
    match cardOption with
    | None -> ""
    | Some card ->
      match card.Position with
      | FaceDown -> ""
      | FaceUp -> "flip"

  let gameCardClasses = sprintf "flippableCard tinyGameCard %s" flipClassName |> Class :> IHTMLProp |> List.singleton
  gameCardClasses

let viewTinyCard (model : Model) dispatch (cardOption:Card option) hideOnMobile =
  match model.GameState with
  | NewGame _
  | Finished _
  | Running (ServerType _) -> div [] []
  | Running (ClientType runningGame) ->
    let cardProps = getTinyCardProps cardOption      
    let spriteClassName =
      match cardOption with
      | None -> ""
      | Some card ->
        match card.Position with
        | FaceDown -> ""
        | FaceUp -> Golf.getClassName "css-sprite-TinyCard" card

    let columnProps =
      if hideOnMobile
      then [ Modifier.IsHidden (Screen.Mobile, true)]
      else [ ]
    Content.content [ Content.Props cardProps; Content.Modifiers columnProps ] 
          [ div [ Class "face front" ] [ ]
            div [ Class (sprintf "face back %s" spriteClassName) ] [ ] ]

let viewTinyCards model dispatch cards = 
  let cardsView =
    cards 
    |> List.map Some
    |> List.mapi (fun i c -> 
      let hideOnMobile = true
      Column.column [] [ viewTinyCard model dispatch c hideOnMobile ]
    )
  Columns.columns [ Columns.IsGap (Screen.All, Columns.Is1) ] cardsView

let getCardProps dispatch clientId (runningGame : ClientRunningGame) playArea cardOption =
  let currentPlayerIsLocal = runningGame.CurrentPlayer.IsSome && runningGame.CurrentPlayer.Value.ClientId = clientId
  let dropableDestination =
    match currentPlayerIsLocal, playArea, cardOption with
    | false, _, _ -> None
    | true, PlayArea.DiscardPile, _ -> Some DropDestination.DiscardPile
    | true, PlayArea.Row1, Some card -> Some (DropDestination.Row1 card)
    | true, PlayArea.Row2, Some card -> Some (DropDestination.Row2 card)
    | true, PlayArea.Row1, None -> None
    | true, PlayArea.Row2, None -> None
    | true, PlayArea.Hand, _ -> None
    | true, PlayArea.DrawPile, _ -> None
  let droppableProps =
    dropableDestination
    |> Option.map (fun dropDestination ->
        [ OnDragOver (fun ev -> ev.preventDefault()) :> IHTMLProp
          OnDrop (fun ev -> DropOn dropDestination |> ServerMsg.RunningGameMsg |> OutgoingMessage |> dispatch) :> IHTMLProp ]
    )
    |> Option.defaultValue ([])
 
  let moveAbleProps =
    let handHasCard = runningGame.CurrentPlayer |> Option.map (fun p -> p.Cards.Hand.IsSome) |> Option.defaultValue false
    let isDraggable = 
      match currentPlayerIsLocal, cardOption with
      | false, _ -> false
      | _, None -> false
      | true, Some card ->
        match card.Position with
        | FaceDown -> false
        | FaceUp -> 
          match handHasCard, playArea with
          | true, PlayArea.Hand -> true
          | true, _ -> false
          | false, _ -> true

    match isDraggable, runningGame.MovingCard, cardOption with
    | false,_,_ -> []
    | true, None, Some card ->
      [ Draggable isDraggable :> IHTMLProp
        OnDragStart (fun ev ->
          ev.dataTransfer.setData("text/plain","dummy") |> ignore
          Move card |> ServerMsg.RunningGameMsg |> OutgoingMessage |> dispatch) :> IHTMLProp ]
    | true, Some movingCard, Some card when card = movingCard ->
      [ Draggable isDraggable :> IHTMLProp
        OnDragEnd (fun _ -> CancelMove |> ServerMsg.RunningGameMsg |> OutgoingMessage |> dispatch) :> IHTMLProp ]
    | true, _, _ -> []

  let clickProps = 
    match cardOption with
    | None ->
      match currentPlayerIsLocal, dropableDestination with
      | false, _ -> []
      | _, None -> []
      | true, Some dropDestination -> [ OnClick (fun e -> DropOn dropDestination |> ServerMsg.RunningGameMsg |> OutgoingMessage |> dispatch) :> IHTMLProp ]
    | Some card ->
      match currentPlayerIsLocal, card.Position, runningGame.MovingCard, dropableDestination with
      | true, FaceDown, None, _ -> [ OnClick (fun e -> FlipCard card |> ServerMsg.RunningGameMsg |> OutgoingMessage |> dispatch) :> IHTMLProp ]
      | true, FaceUp, None, _ -> [ OnClick (fun e -> Move card |> ServerMsg.RunningGameMsg |> OutgoingMessage |> dispatch) :> IHTMLProp ]
      | true, FaceUp, Some m, _ when card = m -> [ OnClick (fun e -> CancelMove |> ServerMsg.RunningGameMsg |> OutgoingMessage |> dispatch) :> IHTMLProp ]
      | true, FaceDown, _, Some dropDestination
      | true, FaceUp, _, Some dropDestination -> [ OnClick (fun e -> DropOn dropDestination |> ServerMsg.RunningGameMsg |> OutgoingMessage |> dispatch) :> IHTMLProp ]
      | true, FaceDown, Some m, None -> []
      | true, FaceUp, Some m, None -> []
      | false, _, _, _ -> []

  let flipClassName =
    match cardOption with
    | None -> ""
    | Some card ->
      match card.Position with
      | FaceDown -> ""
      | FaceUp -> "flip"

  let emptyPositionClassName =
    match cardOption with
    | None -> "emptyCardPosition"
    | Some _ -> ""

  let gameCardClasses = sprintf "flippableCard gameCard %s %s" flipClassName emptyPositionClassName |> Class :> IHTMLProp |> List.singleton
  gameCardClasses ++ clickProps ++ moveAbleProps ++ droppableProps

let viewCard (model : Model) dispatch playArea (cardOption:Card option) hideOnMobile =
  match model.GameState with
  | NewGame _
  | Finished _
  | Running (ServerType _) -> div [] []
  | Running (ClientType runningGame) ->
    let cardProps = getCardProps dispatch model.ClientId runningGame playArea cardOption
    let movingClassName =
      match runningGame.MovingCard, cardOption with
      | Some movingCard, Some card when card = movingCard -> "movingCard"
      | _ -> ""
      
    let spriteClassName =
      match cardOption with
      | None -> ""
      | Some card ->
        match card.Position with
        | FaceDown -> ""
        | FaceUp -> Golf.getClassName "css-sprite-Card" card

    let columnProps =
      if hideOnMobile
      then [ Modifier.IsHidden (Screen.Mobile, true)]
      else [ ]
    Content.content [ Content.Props cardProps; Content.Modifiers columnProps ] 
          [ div [ Class "face front" ] [ ]
            div [ Class (sprintf "face back %s %s" spriteClassName movingClassName) ] [ ] ]

let viewCards model dispatch playArea cards truncateOnMobile = 
  let cardsView =
    cards 
    |> List.map Some
    |> List.mapi (fun i c -> 
      let hideOnMobile = truncateOnMobile && i > 0
      Column.column [] [ viewCard model dispatch playArea c hideOnMobile ]
    )
  Columns.columns [ ] cardsView

let viewCardPairVertical model dispatch (row1Card, row2Card) = 
  let hideOnMobile = false
  [ Column.column []
      [ Columns.columns []
          [ Column.column [ ]
              [ viewCard model dispatch PlayArea.Row1 (Some row1Card) hideOnMobile ] ]
        Columns.columns []
          [ Column.column [ ]
              [ viewCard model dispatch PlayArea.Row2 (Some row2Card) hideOnMobile ] ] ] ]

let viewCardPairsVertical model dispatch row1Cards row2Cards =
  Columns.columns [ Columns.CustomClass "is-hidden-mobile"; Columns.IsGap (Screen.All, Columns.Is2) ]
    (List.zip row1Cards row2Cards |> List.collect (fun p -> viewCardPairVertical model dispatch p))

let viewCardPairHorizontal model dispatch (row1Card, row2Card) = 
  let hideOnMobile = false
  Columns.columns [ Columns.IsMobile; Columns.CustomClass "is-hidden-tablet" ]
    [ Column.column [ Column.Width (Screen.All, Column.IsHalf) ]
        [ viewCard model dispatch PlayArea.Row1 (Some row1Card) hideOnMobile ]
      Column.column [ Column.Width (Screen.All, Column.IsHalf) ]
        [ viewCard model dispatch PlayArea.Row2 (Some row2Card) hideOnMobile ] ]

let viewCardPairsHorizontal model dispatch row1Cards row2Cards =
  List.zip row1Cards row2Cards |> List.map (fun p -> viewCardPairHorizontal model dispatch p)

let viewFinalCards cards = 
  cards |> List.map (fun card -> 
    Column.column [ Column.Width (Screen.All, Column.Is1) ] 
      [ div [ ClassName "flippableCard gameCard flip" ]
          [ div [ Class "face front" ] [ ]
            div [ Class (sprintf "face back %s" (Golf.getClassName "css-sprite-Card" card)) ] [ ]  ] ] )

let [<Literal>] ESC_KEY = 27.
let [<Literal>] ENTER_KEY = 13.

let internal onEnter msg dispatch =
  function
  | (ev:Browser.Types.KeyboardEvent) when ev.keyCode = ENTER_KEY ->
    ev.target?value <- ""
    dispatch msg
  | _ -> ()
  |> OnKeyDown

let newGameView (model: Model) dispatch =
  match model.GameState with
  | Running _
  | Finished _ -> div [] []
  | NewGame newGame ->
    let hasLocalPlayers = newGame.Players |> List.exists (fun x -> x.ClientId = model.ClientId)
    let allPlayersReady = newGame.Players |> List.forall (fun x -> x.Status = NewPlayerStatus.Ready)
    Columns.columns []
      [ Column.column []
          [ Card.card []
              [ Card.content []
                  [ Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Left) ] ]
                      [ Heading.h3 [] [ str (sprintf "Players:") ] ]
                    for x in newGame.Players do
                      let isLocalPlayer = x.ClientId = model.ClientId
                      Columns.columns []
                          [ Column.column [Column.Width (Screen.All, Column.IsNarrow)] 
                              [ match x.Status with
                                | NewPlayerStatus.Joined ->
                                  match isLocalPlayer with
                                  | true ->
                                    Button.button 
                                        [ Button.Color IsLink
                                          Button.OnClick (fun _ -> dispatch ((SetPlayerStatus (x.Id, NewPlayerStatus.Ready)) |> ServerMsg.NewGameMsg |> OutgoingMessage))  ]
                                          [ str "Ready?" ]
                                  | false ->
                                    Button.button 
                                        [ Button.Color IsLink ]
                                          [ str "Waiting" ]
                                | NewPlayerStatus.Ready ->
                                  Button.button 
                                        [ Button.Color IsSuccess ]
                                          [ str "Ready" ]
                              ]
                            Column.column [ ] [ str x.Name ] ]
                    
                    Field.div [ Field.IsGrouped ]
                      [ Control.div [ Control.HasIconLeft; ]
                          [ Input.text [ Input.Color IsPrimary
                                         Input.Placeholder "Who wants to join?"
                                         Input.ValueOrDefault newGame.NewUserName
                                         Input.Props [(onEnter JoinGame dispatch :> IHTMLProp); AutoFocus true; ]
                                         Input.OnChange (fun ev -> !!ev.target?value |> UpdateUserName |> dispatch) ]
                            Icon.icon [ Icon.Size IsSmall; Icon.IsLeft ]
                              [ Fa.i [ Fa.Solid.User ] [ ] ] ]
                        Control.div [ ]
                          [ Button.button [ Button.Color IsPrimary
                                            Button.OnClick (fun _ -> dispatch JoinGame) ]
                              [ str "Join" ] ] ] ] ] ]
        Column.column []
          [ Card.card []
              [ Card.content []
                  [ Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Left) ] ]
                      [ Content.content [ ]
                          [ Heading.h3 [] [ str "Game play:" ]
                            p [] [ str "The game consists of 9 rounds. The lowest score wins. Each player's hand contains 2 rows of 6 cards face down. To start each hand, turn up 1 card in each row. The goal is to match card ranks in each column." ]
                            h2 [] [ str "Scoring" ]
                            h3 [] [ str "Single column scoring" ]
                            ul []
                              [ li [] [ str "Wild cards -2 pts each" ]
                                li [] [ str "Matching pair of any non wild rank score 0 pts" ]
                                li [] [ str "Rank of numbers that do not have a match are added against you." ]
                                ul [] 
                                  [ li [] [ str "1 through 10 face value pts" ]
                                    li [] [ str "Jack 11 pts" ]
                                    li [] [ str "Queen 12 pts" ]
                                    li [] [ str "King 0 pts" ] ] ]
                            h3 [] [ str "Multiple column scoring" ]
                            ul []
                              [ li [] [ str "4 wild cards together -20 pts" ]
                                li [] [ str "6 wild cards together -40 pts" ]
                                li [] [ str "4 of any non wild rank together -10 pts" ] ] ] ] ] ] ] ] 

let finishedGameView (model: Model) dispatch =
  match model.GameState with
  | Running _
  | NewGame _ -> div [] []
  | Finished finishedGame ->
    let sortedPlayers = finishedGame.Players |> List.sortBy (fun x -> x.Score)
    div []
      [ Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Left) ] ]
          [ Heading.h3 [] [ str (sprintf "Players: ") ] ]
        for x in sortedPlayers do
          Columns.columns []
              [ Column.column [] [ str x.Name ]
                Column.column [] [ str (sprintf "This hand: %i" (x.FinalScore |> Option.defaultValue 0)) ]
                Column.column [] [ str (sprintf "Total: %i" x.Score) ] ]
          Columns.columns []
            (viewFinalCards x.Cards.Row1)
          Columns.columns []
            (viewFinalCards x.Cards.Row2)
        Field.div [ Field.IsGrouped ]
          [ Control.div [ ]
              [ Button.button [ Button.Color IsLink
                                Button.OnClick (fun _ -> Deal |> ServerMsg.NewGameMsg |> OutgoingMessage |> dispatch)  ]
                  [ str "Deal" ] ] ] ]

let handCardsView model dispatch =
  match model.GameState with
  | NewGame _
  | Finished _
  | Running (ServerType _) -> div [] []
  | Running (ClientType runningGame) ->
    match runningGame.CurrentPlayer with
    | None -> viewCard model dispatch PlayArea.Hand None false
    | Some player ->
        viewCard model dispatch PlayArea.Hand player.Cards.Hand false

let runningGameView model dispatch =
  match model.GameState with
  | NewGame _
  | Finished _ 
  | Running (ServerType _) -> div [] []
  | Running (ClientType runningGame) ->
    let discardCardView =
      runningGame.DiscardPile
      |> List.tryHead
      |> (fun c -> viewCard model dispatch PlayArea.DiscardPile c false )
  
    let drawPileCardsView = viewCards model dispatch PlayArea.DrawPile runningGame.DrawPile true
    Container.container [Container.IsFluid; ]
        [ Columns.columns []
            [ Column.column [ Column.Width (Screen.All, Column.Is8) ] 
                [ Columns.columns [ ]
                    [ Column.column [ Column.Width (Screen.All, Column.Is4) ] 
                        [ Columns.columns []
                            [ Column.column [ ] 
                                [ Columns.columns []
                                    [ Column.column [ ]
                                        [ Heading.h6 [] [ str ("Hand ") ] ] ]
                                  Columns.columns []
                                    [ Column.column [ ] 
                                        [ handCardsView model dispatch ] ] ] 
                              Column.column [ ] 
                                [ Columns.columns []
                                    [ Column.column [ ]
                                        [ Heading.h6 [] [ str ("Discard ") ] ] ]
                                  Columns.columns []
                                    [ Column.column [ ] 
                                        [ discardCardView ] ] ] ] ] 
                      Column.column [ Column.Width (Screen.All, Column.Is8) ] 
                        [ Columns.columns []
                            [ Column.column []
                                [ Heading.h6 [] [ str ("Draw") ] ] ]
                          Columns.columns []
                            [ Column.column []
                                [ drawPileCardsView ] ] ] ]

                  match runningGame.CurrentPlayer with
                  | None -> ()
                  | Some player ->
                    Columns.columns []
                      [ Column.column []
                          [ Heading.h6 [] [ str (sprintf "Player: %s" player.Name) ] ] ]
                    // visible when on mobile
                    viewCardPairsVertical model dispatch player.Cards.Row1 player.Cards.Row2
                    // visible when not on mobile
                    yield! (viewCardPairsHorizontal model dispatch player.Cards.Row1 player.Cards.Row2)
                    Button.button 
                      [ Button.Color IsDanger
                        Button.OnClick (fun _ -> ServerMsg.QuitGame player.Id |> OutgoingMessage |> dispatch )]
                          [ Icon.icon [ Icon.Size IsSmall; Icon.IsLeft ] [ Fa.i [ Fa.Solid.Trash ] [ ] ]
                            span [] [ str "Quit game" ] ]
                    Button.button 
                      [ Button.Color IsDanger
                        Button.OnClick (fun _ -> ServerMsg.RecoverGame model.GameState |> OutgoingMessage |> dispatch )]
                          [ Icon.icon [ Icon.Size IsSmall; Icon.IsLeft ] [ Fa.i [ Fa.Solid.Sync ] [ ] ]
                            span [] [ str "Recover game" ] ] ]
              Column.column [Column.Width (Screen.All, Column.Is4); Column.Modifiers [ Modifier.IsHidden (Screen.Mobile, true)] ]
                [ Columns.columns [] 
                    [ Heading.h5 [] [ str "All player cards" ] ]
                  for p in runningGame.Players do
                    Columns.columns [] 
                      [ Column.column [] 
                          [ Heading.h6 [] [ str p.Name ] ] ]
                    Columns.columns [] 
                      [ Column.column [] 
                          [ viewTinyCards model dispatch p.Cards.Row1 ] ]
                    Columns.columns [] 
                      [ Column.column [] 
                          [ viewTinyCards model dispatch p.Cards.Row2 ] ] ] ] ]

let drawStatus connectionState =
    Tag.tag [
        Tag.Color
            (match connectionState with
             | DisconnectedFromServer -> Color.IsDanger
             | Connecting -> Color.IsWarning
             | ConnectedToServer _ -> Color.IsSuccess)
    ] [
        match connectionState with
        | DisconnectedFromServer -> str "Disconnected from server"
        | Connecting -> str "Connecting..."
        | ConnectedToServer _ -> str "Connected to server"
    ]
                                     
let view (model : Model) (dispatch : Msg -> unit) =
    Hero.hero [ Hero.IsFullHeight; Hero.CustomClass "playArea" ]
        [ Navbar.navbar [ Navbar.Color IsPrimary ]
            [ Navbar.Item.div [ ]
                [ Heading.h2 [ ]
                    [ str "Golf" ] ] ]
          match model.GameState with
          | NewGame _ -> 
            Container.container [Container.IsFluid]
              [ newGameView model dispatch ]
          | Running _ ->
            Hero.hero [ Hero.IsFullHeight; Hero.CustomClass "playArea" ] 
              [ runningGameView model dispatch ]
          | Finished _ ->
            Hero.hero [ Hero.IsFullHeight; Hero.CustomClass "playArea" ] 
              [ Container.container [Container.IsFluid;]
                  [ finishedGameView model dispatch ] ] 
          drawStatus model.ConnectionState          
        ]

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
|> Program.withSubscription subscription
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactBatched "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
