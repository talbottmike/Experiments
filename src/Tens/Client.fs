module Client

open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Fetch.Types
open Thoth.Fetch
open Fulma
open Fulma.Extensions.Wikiki
open Thoth.Json
open System
open Shared
open Fable.Core.JsInterop
open Fable.FontAwesome
open Fable.Core
open Fable.Import
open Fable.Core.JS

type DealerId = int
type Card = { Id : Guid; Value : int; }
type Player = { Id : int; Name : string; Score : int; Difficulty : int; }
type NewPlayer = { Name : string; Difficulty : int; }
type NewGame = { Players : NewPlayer list; NewUserName : string; NewDifficulty : string }
type RunningGame = { Players : Player list; CurrentPlayer : Player option; CurrentHandScore : int; VisibleCards : Card list; SelectedCards : Card list; DealerId : DealerId option; }
type FinishedGame = { Players : Player list; }

type Model = 
  | NewGame of NewGame
  | Finished of FinishedGame
  | Running of RunningGame

type Msg =
  | JoinGame
  | StartTurn of DealerId
  | Deal
  | DealCard of Card
  | SelectCard of Card
  | UpdateUserName of string
  | UpdateUserDifficulty of string
  | EndTurn
  | GameOver

let private random = System.Random()

let probabilities = [
  (1, 0.15)
  (2, 0.15)
  (3, 0.13)
  (4, 0.12)
  (5, 0.11)
  (6, 0.1)
  (7, 0.09)
  (8, 0.08)
  (9, 0.07)
]

let potentials = 
  probabilities 
  |> List.fold 
    (fun potentials (v, probability) -> [1. .. (probability * 100.)] |> List.map (fun _ -> v) |> List.append potentials)
    []

let removeCard cards card = cards |> List.filter (fun c -> c <> card)

let private startTurn difficulty = 
  let start dispatch =
    let interval = 
      match difficulty with
      | v when v < 25 -> 2000
      | v when v <= 50 -> 1200
      | v when v <= 75 -> 1000
      | _ -> 800
    let dealerId = setInterval (fun () -> dispatch (DealCard ({ Id = Guid.NewGuid(); Value = potentials.[random.Next(0,potentials.Length - 1)]}))) interval
    StartTurn dealerId |> dispatch

  Cmd.ofSub start

let private stopTurn id = 
  let stop _ =
    clearInterval id
    ()

  Cmd.ofSub stop

let init () : Model * Cmd<Msg> =
  let model = NewGame { NewGame.Players = []; NewUserName = ""; NewDifficulty = "50"; }
  model, Cmd.none
  
let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
  match currentModel, msg with
  | NewGame model, JoinGame ->
    let difficulty =
      match Int32.TryParse model.NewDifficulty with
      | true, v -> v
      | false, v -> 16
    let newPlayer = { Name = model.NewUserName; Difficulty = difficulty }
    let players = if String.IsNullOrWhiteSpace model.NewUserName then model.Players else newPlayer :: model.Players
    NewGame { model with Players = players; NewUserName = ""; NewDifficulty = "50"; }, Cmd.none
  | NewGame model, UpdateUserName v ->
    NewGame { model with NewUserName = v; }, Cmd.none
  | NewGame model, UpdateUserDifficulty v ->
    NewGame { model with NewDifficulty = v; }, Cmd.none
  | NewGame model, Deal when model.Players.Length > 0 ->
    let addPlayer i x = { Player.Id = i; Name = x.Name; Difficulty = x.Difficulty; Score = 0; }
    let players = model.Players |> List.mapi addPlayer
    let newModel = Running { Players = players; CurrentPlayer = players |> List.tryHead; CurrentHandScore = 0; VisibleCards = []; SelectedCards = []; DealerId = None; }
    newModel, Cmd.none
  | Running model, _ ->
    match model.CurrentPlayer, msg with
    | None, _ -> 
      Running model, Cmd.none
    | Some player, Deal ->
      Running model, startTurn player.Difficulty
    | Some player, EndTurn ->
      let playersUpdatedFromPreviousTurn = model.Players |> List.map (fun x -> if x.Id = player.Id then { x with Score = x.Score + model.CurrentHandScore } else x) |> List.sortBy (fun x -> x.Id)
      let nextPlayer = model.Players |> List.sortBy (fun x -> x.Id) |> List.tryFind (fun x -> x.Id > player.Id) 
      let newCurrentPlayer = nextPlayer |> Option.orElse (model.Players |> List.tryHead)
      let newModel =
        let playerIds = model.Players |> List.map (fun x -> sprintf "%i" x.Id) |> String.concat ", "
        let logMsg = sprintf "Ended turn with PlayerIds: %s CurrentPlayerId: %i" playerIds player.Id
        { model with DealerId = None; Players = playersUpdatedFromPreviousTurn; CurrentPlayer = newCurrentPlayer; CurrentHandScore = 0; VisibleCards = []; SelectedCards = []; }
      
      let cmd = 
          match nextPlayer, model.DealerId with
          | None, None -> Cmd.ofMsg GameOver
          | None, Some dealerId -> Cmd.batch [ stopTurn dealerId; Cmd.ofMsg GameOver ]
          | Some _, Some dealerId -> stopTurn dealerId
          | Some _, None -> Cmd.none

      Running newModel, cmd 
    | Some player, StartTurn dealerId ->
      Running { model with DealerId = Some dealerId }, Cmd.none
    | Some player, DealCard card ->
      let cmd = if model.VisibleCards.Length > 10 then Cmd.ofMsg EndTurn else Cmd.none

      Running { model with VisibleCards = model.VisibleCards @ [card] }, cmd
    | Some player, SelectCard card ->
      let newModel, cmd = 
        let selectedCards = card :: model.SelectedCards
        let selectedCount = selectedCards.Length
        let remainingCards = removeCard model.VisibleCards card
        let selectedValues = selectedCards |> List.sumBy (fun x -> x.Value)
        match selectedCount, selectedValues with
        | _, v when v > 10 -> model, Cmd.ofMsg EndTurn
        | v, _ when v > 3 -> model, Cmd.ofMsg EndTurn
        | 3, v when v < 10 -> model, Cmd.ofMsg EndTurn
        | 2, 10
        | 3, 10 -> { model with CurrentHandScore = model.CurrentHandScore + 1; VisibleCards = remainingCards; SelectedCards = []; }, Cmd.none
        | _ -> { model with VisibleCards = remainingCards; SelectedCards = selectedCards; }, Cmd.none
      Running newModel, cmd
    | _, GameOver ->
      let newModel = Finished { Players = model.Players }
      newModel, model.DealerId |> Option.map (stopTurn) |> Option.defaultValue Cmd.none
    | _ -> Running model, Cmd.none
  | Finished model, Deal ->
    let newPlayer = model.Players |> List.tryHead
    let cmd = newPlayer |> Option.map (fun x -> startTurn x.Difficulty) |> Option.defaultValue Cmd.none
    let newModel = { Players = model.Players; CurrentPlayer = newPlayer; CurrentHandScore = 0; VisibleCards = []; SelectedCards = []; DealerId = None; }
    Running newModel, cmd
  | _ -> currentModel, Cmd.none

let (++) = List.append

let viewCard dispatch (card:Card) =
  
  // Column.column [ Column.Width (Screen.All, Column.Is1); Column.Modifiers [ Modifier.IsHiddenOnly (Screen.Tablet, true) ] ] 
  //   [ Button.button 
  //       [ Button.Size IsMedium
  //         Button.OnClick (fun _ -> dispatch (SelectCard card))
  //         Button.Props [ Style [ Padding "2rem"; FontSize "3rem"; ] ]
  //     ] [ Text.span [ Modifiers [ Modifier.TextWeight TextWeight.Bold ] ] [ str (sprintf "%i" card.Value) ] ] ]
  Button.button 
        [ Button.Modifiers [ ]
          Button.Size IsSmall
          Button.OnClick (fun _ -> dispatch (SelectCard card))
          Button.Props [ Style [ FontSize "2rem"; ] ]
      ] [ Text.span [ Modifiers [ Modifier.TextWeight TextWeight.Bold ] ] [ str (sprintf "%i" card.Value) ] ]
    

let viewCards dispatch cards = 
    cards |> List.map (viewCard dispatch)
         
let [<Literal>] ESC_KEY = 27.
let [<Literal>] ENTER_KEY = 13.
let internal onEnter msg dispatch =
    function
    | (ev:Browser.Types.KeyboardEvent) when ev.keyCode = ENTER_KEY ->
        ev.target?value <- ""
        dispatch msg
    | _ -> ()
    |> OnKeyDown

let newGameView (model: NewGame) dispatch =
  div []
    [ Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Left) ] ]
        [ Heading.h3 [] [ str (sprintf "Players: ") ] ]
      for x in model.Players do
        Columns.columns []
            [ Column.column [] [ str x.Name ] ]
      div []
        [
          Field.div [ ]
            [ Label.label [ ]
                    [ str "Name" ]
              Control.div [ Control.HasIconLeft; ]
                [ Input.text [ Input.Color IsPrimary
                               Input.Placeholder "Who wants to join?"
                               Input.ValueOrDefault model.NewUserName
                               Input.Props [(onEnter JoinGame dispatch :> IHTMLProp); AutoFocus true; ]
                               Input.OnChange (fun ev -> !!ev.target?value |> UpdateUserName |> dispatch) ]
                  Icon.icon [ Icon.Size IsSmall; Icon.IsLeft ]
                    [ Fa.i [ Fa.Solid.User ] [ ] ]
                  
                  // Input.number [ Input.Color IsPrimary
                  //                Input.Placeholder "How old are you?"
                  //                Input.ValueOrDefault model.NewDifficulty
                  //               //  Input.Props [(onEnter JoinGame dispatch :> IHTMLProp); AutoFocus true; ]
                  //                Input.OnChange (fun ev -> !!ev.target?value |> UpdateUserDifficulty |> dispatch) ]
                  Icon.icon [ Icon.Size IsSmall; Icon.IsLeft ]
                    [ Fa.i [ Fa.Solid.User ] [ ] ] ]
              Label.label [ ]
                    [ str "Difficulty" ]
              Control.div [ ]
                [ div [ ]
                    [ Slider.slider [ Slider.Color IsPrimary
                                      Slider.Size IsLarge
                                      Slider.OnChange (fun ev -> !!ev.target?value |> UpdateUserDifficulty |> dispatch) ]
                      // div [ ]
                      //   [ str (string model.NewDifficulty) ]
                    ]]
              Control.div [ ]
                [ 
                  Button.button [ Button.Color IsPrimary
                                  Button.OnClick (fun _ -> dispatch JoinGame)
                                  Button.Disabled (model.NewUserName.Length < 1) ]
                    [ str "Join" ] ] ]
          Field.div [ Field.IsGrouped ]
            [ Control.div [ ]
                [ Button.button [ Button.Color IsLink
                                  Button.OnClick (fun _ -> dispatch Deal)
                                  Button.Disabled (model.Players.Length < 1)  ]
                    [ str "Deal" ] ] ] ] ]

let finishedGameView (model: FinishedGame) dispatch =
  let sortedPlayers = model.Players |> List.sortBy (fun x -> x.Score)
  div []
    [ Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Left) ] ]
        [ Heading.h3 [] [ str (sprintf "Players: ") ] ]
      for x in sortedPlayers do
        Columns.columns []
            [ Column.column [] [ str x.Name ]
              Column.column [] [ str (sprintf "Total: %i" x.Score) ] ]
      Field.div [ Field.IsGrouped; ]
        [ Control.div [ ]
            [ Button.button [ Button.Color IsLink
                              Button.OnClick (fun _ -> dispatch Deal) ]
                [ str "Deal" ] ] ] ]

let runningGameView model dispatch =
  Container.container []
      [ match model.CurrentPlayer, model.DealerId with
        | None,_ -> ()
        | Some player, None -> 
          Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Left) ] ]
              [ Heading.h5 [] [ str (player.Name) ]
                div [] [ str (sprintf "Score: %i" model.CurrentHandScore) ] ]
          Button.button [ Button.Color IsSuccess
                          Button.OnClick (fun _ -> dispatch Deal) ]
                [ str "Start turn" ] 
        | Some player, Some _ ->
          Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Left) ] ]
              [ Heading.h5 [] [ str (player.Name) ]
                div [] [ str (sprintf "Score: %i" model.CurrentHandScore) ] ]
          // Columns.columns []
          //   (viewCards dispatch model.VisibleCards)]
          Button.list []
            (viewCards dispatch model.VisibleCards)]
                                     
let view (model : Model) (dispatch : Msg -> unit) =
    div []
        [ Navbar.navbar [ Navbar.Color IsPrimary ]
            [ Navbar.Item.div [ ]
                [ Heading.h2 [ ]
                    [ str "Tens" ] ] ]

          match model with
          | NewGame newGame -> 
            Container.container []
                [ newGameView newGame dispatch ]
          | Running state ->
            runningGameView state dispatch
            
          | Finished state ->
            Container.container []
                [ finishedGameView state dispatch ] ]

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
// |> Program.withBridgeConfig
//     (
//         Bridge.endpoint "/socket/init"
//         |> Bridge.withMapping Remote
//     )
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactBatched "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
