module GolfClient

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

open Fable.Core
open Fable.Import

// Cards from
// https://opengameart.org/content/vintage-playing-cards?page=3

type Model =
  { ClientId : Guid
    GameState : GameState
    ServerGameState : GameState }

type Msg =
  | JoinGame
  | OutgoingMessage of ServerMsg
  | UpdateUserName of string
  | IncomingMsg of ClientMsg
  
let init () : Model * Cmd<Msg> =
  let gameState = Golf.blankGame
  let model =
    { ClientId = Guid.NewGuid()
      GameState = gameState
      ServerGameState = gameState }
  model, Cmd.none
  
let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
  match msg with
  | IncomingMsg (ClientMsg.RunningGameMsg AbortedGame) ->
    currentModel, Cmd.none //Cmd.ofMsg (OutgoingMessage (RecoverGame currentModel.GameState))
  | OutgoingMessage om ->
    let newServerState, newMsgs = Golf.handleUpdate currentModel.ServerGameState om
    let newCmds = newMsgs |> List.map (IncomingMsg >> Cmd.ofMsg)
    { currentModel with ServerGameState = newServerState }, Cmd.batch newCmds
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

let viewCard (model : Model) dispatch playArea (cardOption:Card option) =
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
        | FaceUp -> Golf.getClassName card

    Column.column [ Column.Width (Screen.All, Column.Is1); ] 
      [ div cardProps 
          [ div [ Class "face front" ] [ ]
            div [ Class (sprintf "face back %s %s" spriteClassName movingClassName) ] [ ] ] ]

let viewCards model dispatch playArea cards = 
    cards |> List.map (Some >> viewCard model dispatch playArea)
         
let viewFinalCards cards = 
  cards |> List.map (fun card -> 
    Column.column [ Column.Width (Screen.All, Column.Is1) ] 
      [ div [ ClassName "flippableCard gameCard flip" ]
          [ div [ Class "face front" ] [ ]
            div [ Class (sprintf "face back %s" (Golf.getClassName card)) ] [ ]  ] ] )

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
    div []
      [ Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Left) ] ]
          [ Heading.h3 [] [ str (sprintf "Players: ") ] ]
        for x in newGame.Players do
          let isLocalPlayer = x.ClientId = model.ClientId
          Columns.columns []
              [ Column.column [] [ str x.Name ]
                Column.column [] 
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
                  ] ]
        
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
                  [ str "Join" ] ] ] 
        
        div [ ]
          [ Content.content [ ]
              [ h2 [] [ str "Game play" ]
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
                    li [] [ str "4 of any non wild rank together -10 pts" ] ] ] ] ]

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
    | None -> viewCard model dispatch PlayArea.Hand None
    | Some player ->
        viewCard model dispatch PlayArea.Hand player.Cards.Hand

let discardCardsView (model : Model) dispatch =
  match model.GameState with
  | NewGame _
  | Finished _ 
  | Running (ServerType _) -> []
  | Running (ClientType runningGame) ->
    if runningGame.DiscardPile.Length > 0 
    then viewCards model dispatch PlayArea.DiscardPile (runningGame.DiscardPile |> List.truncate 5)
    else [ viewCard model dispatch PlayArea.DiscardPile None ]

let runningGameView model dispatch =
  match model.GameState with
  | NewGame _
  | Finished _ 
  | Running (ServerType _) -> div [] []
  | Running (ClientType runningGame) ->
    let handAndDiscardView = ( (handCardsView model dispatch) :: (discardCardsView model dispatch) )
    Container.container [Container.IsFluid; Container.CustomClass "playArea" ]
        [ Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Left) ] ]
              [ Heading.h5 [] [ str ("Draw pile:") ] ]
          Columns.columns []
            (viewCards model dispatch PlayArea.DrawPile runningGame.DrawPile )
          Columns.columns []
            [ Column.column [ Column.Width (Screen.All, Column.Is1)  ]
                [ Heading.h5 [] [ str ("Hand: ") ] ]
              Column.column [ Column.Width (Screen.All, Column.Is5) ]
                  [ Heading.h5 [] [ str ("Discard pile:") ] ] ] 
          Columns.columns []
            handAndDiscardView
          match runningGame.CurrentPlayer with
          | None -> ()
          | Some player ->
            Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Left) ] ]
                [ Heading.h5 [] [ str (player.Name) ] ]
            Columns.columns []
              (viewCards model dispatch PlayArea.Row1 player.Cards.Row1)
            Columns.columns []
              (viewCards model dispatch PlayArea.Row2 player.Cards.Row2) 
          // Button.button 
          //   [ Button.Color IsDanger
          //     Button.OnClick (fun _ -> ServerRunningGameMsg.RequestState |> ServerMsg.RunningGameMsg |> OutgoingMessage |> dispatch )]
          //       [ Icon.icon [ Icon.Size IsSmall; Icon.IsLeft ] [ Fa.i [ Fa.Solid.Sync ] [ ] ]
          //         span [] [ str "Sync" ] ] 
                  ]
                                     
let view (model : Model) (dispatch : Msg -> unit) =
    div []
        [ Navbar.navbar [ Navbar.Color IsPrimary ]
            [ Navbar.Item.div [ ]
                [ Heading.h2 [ ]
                    [ str "Golf" ] ] ]
          match model.GameState with
          | NewGame _ -> 
            Container.container [Container.IsFluid]
                [ newGameView model dispatch ]
          | Running _ ->
            Hero.hero [ Hero.IsFullHeight; Hero.CustomClass "playArea" ] [
              Container.container [Container.IsFluid;]
                  [ runningGameView model dispatch ] 
            ]
          | Finished _ ->
            Hero.hero [ Hero.IsFullHeight; Hero.CustomClass "playArea" ] [
              Container.container [Container.IsFluid;]
                  [ finishedGameView model dispatch ] 
            ]
        ]

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactBatched "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
