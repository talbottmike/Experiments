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

open Fable.Core
open Fable.Import

// Cards from
// https://opengameart.org/content/vintage-playing-cards?page=3
type CardSuit =
  | Clubs
  | Diamonds
  | Spades
  | Hearts with
  static member allValues = [ Clubs; Hearts; Spades; Diamonds; ]

type CardRank =
  | Two 
  | Three 
  | Four 
  | Five 
  | Six 
  | Seven 
  | Eight 
  | Nine 
  | Ten 
  | Jack 
  | Queen 
  | King 
  | Ace
  | Joker with
  member this.toInt () =
    match this with
    | Two -> 2
    | Three -> 3
    | Four -> 4
    | Five -> 5
    | Six -> 6
    | Seven -> 7
    | Eight -> 8
    | Nine -> 9
    | Ten -> 10
    | Jack -> 11
    | Queen -> 12
    | King -> 12
    | Ace -> 1
    | Joker -> -2

  static member allValues = 
    [ Two 
      Three 
      Four 
      Five 
      Six 
      Seven 
      Eight 
      Nine 
      Ten 
      Jack 
      Queen 
      King 
      Ace
      Joker ] 

type CardValue = (CardSuit * CardRank)

type CardPosition =
  | FaceUp
  | FaceDown

type Card = { Id : Guid; Value : CardValue; Position : CardPosition; Selected : bool; }
type PlayerCards = { Row1 : Card list; Row2 : Card list; Hand : Card option; }
type Player = { Id : int; Name : string; Cards : PlayerCards; FinalScore : int option; Score : int; }
type NewGame = { Names : string list; NewUserName : string }
type RunningGame = { DrawPile : Card list; DiscardPile : Card list; Players : Player list; MovingCard : Card option; IsDebug : bool; CurrentPlayer : Player option; Log : string list; }
type FinishedGame = { Players : Player list; }

type PlayArea =
  | DrawPile
  | DiscardPile
  | Row1
  | Row2
  | Hand

type DropDestination =
  | DiscardPile
  | Row1 of Card
  | Row2 of Card

type Model = 
  | NewGame of NewGame
  | Finished of FinishedGame
  | Running of RunningGame

type Msg =
  | JoinGame
  | Deal
  | FlipCard of Card
  | CancelMove
  | Move of Card
  | DropOn of DropDestination
  | EndTurn
  | UpdateUserName of string
  | GameOver
  
let singleDeckOfCards =
  CardRank.allValues |> List.collect (fun r -> CardSuit.allValues |> List.map (fun s -> (s,r)))

let deckOfCards =
  [ 1..4 ] 
  |> List.collect (fun _ -> singleDeckOfCards)
  |> List.map (fun v -> { Id = Guid.NewGuid(); Value = v; Position = FaceDown; Selected = false; })

let shuffle cards = cards |> List.sortBy (fun _ -> Guid.NewGuid())

let flipAndSelect card =
  let newPosition = 
    match card.Position with
    | FaceUp -> FaceDown
    | FaceDown -> FaceUp
  { card with Position = newPosition; Selected = true; }

let flipUp (card : Card) = { card with Position = FaceUp; }
let flipDown (card : Card) = { card with Position = FaceDown; }

let getUri (card : Card) =
  let suit, rank = card.Value
  let suitUri = sprintf "%A" suit
  let rankUri =
    match rank with
      | Two   -> "2"
      | Three -> "3"
      | Four  -> "4"
      | Five  -> "5"
      | Six   -> "6"
      | Seven -> "7"
      | Eight -> "8"
      | Nine  -> "9"
      | Ten   -> "10"
      | Jack  -> "Jack"
      | Queen -> "Queen"
      | King  -> "King"
      | Ace   -> "Ace"
      | Joker -> "Joker"

  sprintf "/assets/VintagePlayingCards/%s/%s.png" suitUri rankUri
  //sprintf "/assets/VintagePlayingCards/%s/CleanCards/%s.png" suitUri rankUri

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

// let flip card =
//   match card.Position with
//   | FaceUp -> flipUp card
//   | FaceDown -> flipDown card

// let selectCard card = { card with Selected = true; }
let removeCard cards card = cards |> List.filter (fun c -> c <> card)
let replaceCard cards newCard oldCard = cards |> List.map (fun c -> if c = oldCard then newCard else c) 
// let tryFlip cards card = cards |> List.map (fun c -> if c = card then flip c else c )
let tryFlipUp cards card = cards |> List.map (fun c -> if c = card then flipUp c else c )
// let tryFlipAndSelect cards id = cards |> List.map (fun c -> if c.Id = id then flipAndSelect c else c )
// let trySelect cards id = cards |> List.map (fun c -> if c.Id = id then selectCard c else c )
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

let deal (players : Player list) = 
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

  let discardPile, drawPile = [{ undealtCards.Head with Position = FaceUp }], undealtCards.Tail

  let mutable isDebug = false
#if DEBUG
  isDebug <- true
#endif
  let sortedPlayersWithNewCards = playersWithNewCards |> List.sortBy (fun x -> x.Id)
  { DrawPile = drawPile ; DiscardPile = discardPile; Players = sortedPlayersWithNewCards; MovingCard = None; IsDebug = isDebug; CurrentPlayer = sortedPlayersWithNewCards |> List.tryHead; Log = []; }

let init () : Model * Cmd<Msg> =
  let model = NewGame { NewGame.Names = []; NewUserName = ""; }  //deal 0 |> Running
  model, Cmd.none
  
let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
  match currentModel, msg with
  | NewGame model, JoinGame ->
    let names = if String.IsNullOrWhiteSpace model.NewUserName then model.Names else model.NewUserName :: model.Names
    NewGame { model with Names = names; NewUserName = ""; }, Cmd.none
  | NewGame model, UpdateUserName v ->
    NewGame { model with NewUserName = v; }, Cmd.none
  | NewGame model, Deal when model.Names.Length > 0 ->
    let addPlayer i name = { Player.Id = i; Name = name; Cards = { Row1 = []; Row2 = []; Hand = None;}; FinalScore = None; Score = 0; }
    let newModel = model.Names |> List.mapi addPlayer |> deal |> Running
    newModel, Cmd.none 
  | Running model, _ ->
    let cmd = if model.Players |> List.exists (fun x -> x.Cards.Row1 |> List.exists (fun c -> c.Position = FaceDown) || x.Cards.Row2 |> List.exists (fun c -> c.Position = FaceDown)) then Cmd.none else Cmd.ofMsg GameOver
    match model.CurrentPlayer, msg with
    | None, _ -> 
      Running model, cmd
    | Some player, EndTurn ->
      let newModel =
          let playersUpdatedFromPreviousTurn = model.Players |> List.map (fun x -> if x.Id = player.Id then player else x) |> List.sortBy (fun x -> x.Id)
          let newCurrentPlayer = model.Players |> List.sortBy (fun x -> x.Id) |> List.tryFind (fun x -> x.Id > player.Id) |> Option.orElse (model.Players |> List.tryHead)
          let playerIds = model.Players |> List.map (fun x -> sprintf "%i" x.Id) |> String.concat ", "
          let logMsg = sprintf "Ended turn with PlayerIds: %s CurrentPlayerId: %i" playerIds player.Id
          { model with Players = playersUpdatedFromPreviousTurn; CurrentPlayer = newCurrentPlayer; Log = logMsg :: model.Log; }
      Running newModel, cmd
    | Some player, FlipCard card ->
      let newModel = 
        // Don't flip if card in hand
        match player.Cards.Hand.IsSome with
        | true -> model
        | false ->
          getCardPlayArea model card
          |> Option.map (fun playArea -> 
            match playArea with
            | PlayArea.DrawPile ->
              // Don't flip draw pile if any card in draw pile already flipped
              if model.DrawPile |> List.exists (fun c -> c.Position = FaceUp) then model
              else { model with DrawPile = tryFlipUp model.DrawPile card }
            | PlayArea.Row1 ->
              { model with CurrentPlayer = Some { player with Cards = { player.Cards with Row1 = tryFlipUp player.Cards.Row1 card; } } }
            | PlayArea.Row2 ->
              { model with CurrentPlayer = Some { player with Cards = { player.Cards with Row2 = tryFlipUp player.Cards.Row2 card; } } }
            // Don't flip cards in hand or discard pile
            | PlayArea.Hand
            | PlayArea.DiscardPile -> model
          )
          |> Option.defaultValue model
      Running newModel, Cmd.none
    | _, Move selectedCard ->
      Running { model with MovingCard = Some selectedCard }, Cmd.none
    | _, CancelMove ->
      Running {model with MovingCard = None}, Cmd.none
    | Some player, DropOn dropDestination ->
      let newModel, newCmd =
        model.MovingCard
        |> Option.map (fun sourceCard -> 
          getCardPlayArea model sourceCard
          |> Option.map (fun (sourceArea) ->
            match sourceArea, dropDestination with
            | PlayArea.DrawPile, DiscardPile ->
              { model with 
                  DrawPile = removeCard model.DrawPile sourceCard
                  DiscardPile = sourceCard :: (model.DiscardPile |> List.map flipDown)
                  MovingCard = None }, Cmd.ofMsg EndTurn
            | PlayArea.Hand, DiscardPile ->
              { model with 
                  CurrentPlayer = Some { player with Cards = { player.Cards with Hand = None; } }
                  DiscardPile = sourceCard :: (model.DiscardPile |> List.map flipDown)
                  MovingCard = None }, Cmd.ofMsg EndTurn
            | PlayArea.DiscardPile, Row1 destinationCard ->
              { model with 
                  CurrentPlayer = Some { player with Cards = { player.Cards with Row1 = replaceCard player.Cards.Row1 sourceCard destinationCard; Hand = Some (flipUp destinationCard); } }
                  DiscardPile = model.DiscardPile.Tail |> List.mapi (fun i x -> if i = 0 then flipUp x else x)
                  MovingCard = None }, Cmd.none
            | PlayArea.DiscardPile, Row2 destinationCard ->
              { model with 
                  CurrentPlayer = Some { player with Cards = { player.Cards with Row2 = replaceCard player.Cards.Row2 sourceCard destinationCard; Hand = Some (flipUp destinationCard); } }
                  DiscardPile = model.DiscardPile.Tail |> List.mapi (fun i x -> if i = 0 then flipUp x else x)
                  MovingCard = None }, Cmd.none
            | PlayArea.DrawPile, Row1 destinationCard ->
              { model with 
                  CurrentPlayer = Some { player with Cards = { player.Cards with Row1 = replaceCard player.Cards.Row1 sourceCard destinationCard; Hand = Some (flipUp destinationCard); } }
                  DrawPile = removeCard model.DrawPile sourceCard
                  MovingCard = None }, Cmd.none
            | PlayArea.DrawPile, Row2 destinationCard ->
              { model with 
                  CurrentPlayer = Some { player with Cards = { player.Cards with Row2 = replaceCard player.Cards.Row2 sourceCard destinationCard; Hand = Some (flipUp destinationCard); } }
                  DrawPile = removeCard model.DrawPile sourceCard
                  MovingCard = None }, Cmd.none
            | PlayArea.Hand, Row1 destinationCard ->
              { model with 
                  CurrentPlayer = Some { player with Cards = { player.Cards with Row1 = replaceCard player.Cards.Row1 sourceCard destinationCard; Hand = Some (flipUp destinationCard); } }
                  MovingCard = None }, Cmd.none
            | PlayArea.Hand, Row2 destinationCard ->
              { model with 
                  CurrentPlayer = Some { player with Cards = { player.Cards with Row2 = replaceCard player.Cards.Row2 sourceCard destinationCard; Hand = Some (flipUp destinationCard); } }
                  MovingCard = None }, Cmd.none
            | _ -> model, Cmd.none
          )
          |> Option.defaultValue (model, Cmd.none)
        )
        |> Option.defaultValue (model, Cmd.none)
      
      Running newModel, newCmd
    | _, GameOver ->
      let newModel = Finished { Players = model.Players |> List.map scorePlayer }
      newModel, Cmd.none
    | _ -> Running { model with Log = (sprintf "Unhandled msg: %A" msg) :: model.Log }, Cmd.none
  | Finished model, Deal ->
    let newModel = deal model.Players |> Running
    newModel, Cmd.none
  | _ -> currentModel, Cmd.none

let (++) = List.append

let viewCard model dispatch playArea (card:Card) =
    let dropDestinationOption =
      match playArea with
      | PlayArea.DiscardPile -> Some DropDestination.DiscardPile
      | PlayArea.Row1 -> Some (DropDestination.Row1 card)
      | PlayArea.Row2 -> Some (DropDestination.Row2 card)
      | PlayArea.Hand -> None
      | PlayArea.DrawPile -> None

    let isDroppable = dropDestinationOption.IsSome

    let (droppableStyles, (droppableProps:IHTMLProp list)) =
      match dropDestinationOption with
      | None -> [],[]
      | Some dropDestination ->
        if isDroppable then
            [
            ],
            [
                OnDragOver (fun ev -> ev.preventDefault()) 
                OnDrop (fun ev -> Msg.DropOn dropDestination |> dispatch) 
            ]
        else [],[]

    let handHasCard = model.CurrentPlayer |> Option.map (fun p -> p.Cards.Hand.IsSome) |> Option.defaultValue false
    let isDraggable = 
      match card.Position with
      | FaceDown -> false
      | FaceUp -> 
        match handHasCard, playArea with
        | true, PlayArea.Hand -> true
        | true, _ -> false
        | false, _ -> true

    let moveableStyles, (moveAbleProps:IHTMLProp list) =
      match isDraggable, model.MovingCard with
      | false,_ -> [],[]
      | true, None ->
          if true then
              [
              ],
              [
                  Draggable isDraggable 
                  OnDragStart (fun ev ->
                      ev.dataTransfer.setData("text/plain","dummy") |> ignore
                      Msg.Move card |> dispatch) 
              ]
          else 
            [], []
      | true, Some movingCard ->
          if card = movingCard then
              [
                  Opacity "0.1"
              ],
              [
                  Draggable isDraggable 
                  OnDragEnd (fun _ -> Msg.CancelMove |> dispatch) 
              ]
          else [], []    

    let cardHtmlProps = ( Style ( moveableStyles ) :> IHTMLProp :: moveAbleProps ) ++ droppableProps
    let clickEventProp = OnClick (fun _ -> dispatch (FlipCard card)) :> IHTMLProp
    let cardOptions =  Props (clickEventProp :: cardHtmlProps)
    match card.Position with
    | FaceDown -> 
      Column.column [ Column.Width (Screen.All, Column.Is1) ] 
        [ Image.image [ Image.Option.Props (clickEventProp :: cardHtmlProps) ] [ img [ Src "/assets/VintagePlayingCards/BackFace/CardBackFaceWhiteBlueSmallPattern.png" ] ] ]
    | FaceUp -> 
      let uri = getUri card
      Column.column [ Column.Width (Screen.All, Column.Is1) ] 
        [ Image.image [ Image.Option.Props (clickEventProp :: cardHtmlProps) ] [ img [ Src uri ] ] ]

let viewBlankCard model dispatch playArea =
    let dropDestinationOption =
      match playArea with
      | PlayArea.DiscardPile -> Some DropDestination.DiscardPile
      | PlayArea.Row1 -> None
      | PlayArea.Row2 -> None
      | PlayArea.Hand -> None
      | PlayArea.DrawPile -> None

    let isDroppable = dropDestinationOption.IsSome

    let (droppableStyles, (droppableProps:IHTMLProp list)) =
      match dropDestinationOption with
      | None -> [],[]
      | Some dropDestination ->
        if isDroppable then
            [
            ],
            [
                OnDragOver (fun ev -> ev.preventDefault()) 
                OnDrop (fun ev -> Msg.DropOn dropDestination |> dispatch) 
            ]
        else [],[]

    let cardStyle: CSSProp list =
        [
            Opacity "0.3"
        ]
    let cardHtmlProps = ( Style ( cardStyle ) :> IHTMLProp :: droppableProps) 
    Column.column [ Column.Width (Screen.All, Column.Is1) ]
      [ Image.image [ Image.Option.Props cardHtmlProps ] [ img [ Src "/assets/VintagePlayingCards/BackFace/CardBackFaceWhiteBlueSmallPattern.png" ] ] ]

let viewCards model dispatch playArea cards = 
    cards |> List.map (viewCard model dispatch playArea)
         
let viewFinalCards cards = 
  cards |> List.map (fun card -> 
    let uri = getUri card
    Column.column [ Column.Width (Screen.All, Column.Is1) ] 
      [ Image.image [ ] [ img [ Src uri ] ] ])

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
      for x in model.Names do
        Columns.columns []
            [ Column.column [] [ str x ] ]
      Field.div [ Field.IsGrouped ]
        [ Control.div [ Control.HasIconLeft; ]
            [ Input.text [ Input.Color IsPrimary
                           Input.Placeholder "Who wants to join?"
                           Input.ValueOrDefault model.NewUserName
                           Input.Props [(onEnter JoinGame dispatch :> IHTMLProp); AutoFocus true; ]
                           Input.OnChange (fun ev -> !!ev.target?value |> UpdateUserName |> dispatch) ]
              Icon.icon [ Icon.Size IsSmall; Icon.IsLeft ]
                [ Fa.i [ Fa.Solid.User ] [ ] ] ]
          Control.div [ ]
            [ Button.button [ Button.Color IsPrimary
                              Button.OnClick (fun _ -> dispatch JoinGame) ]
                [ str "Join" ] ] ]
      Field.div [ Field.IsGrouped ]
        [ Control.div [ ]
            [ Button.button [ Button.Color IsLink
                              Button.OnClick (fun _ -> dispatch Deal)  ]
                [ str "Deal" ] ] ] ]

let finishedGameView (model: FinishedGame) dispatch =
  let sortedPlayers = model.Players |> List.sortBy (fun x -> x.Score)
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
                              Button.OnClick (fun _ -> dispatch Deal)  ]
                [ str "Deal" ] ] ] ]

let handCardsView model dispatch =
  match model.CurrentPlayer with
  | None -> viewBlankCard model dispatch PlayArea.Hand
  | Some player ->
      match player.Cards.Hand with
      | None ->
        viewBlankCard model dispatch PlayArea.Hand
      | Some x ->
        viewCard model dispatch PlayArea.Hand x

let discardCardsView model dispatch =
  if model.DiscardPile.Length > 0 then
    viewCards model dispatch PlayArea.DiscardPile (model.DiscardPile |> List.truncate 5)
  else 
    [ viewBlankCard model dispatch PlayArea.DiscardPile ]

let runningGameView model dispatch =
  let handAndDiscardView = ( (handCardsView model dispatch) :: (discardCardsView model dispatch) )
  Container.container []
      [ Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Left) ] ]
            [ Heading.h5 [] [ str ("Draw pile:") ] ]
        Columns.columns []
          (viewCards model dispatch PlayArea.DrawPile (model.DrawPile |> List.truncate 6) )
        Columns.columns []
          [ Column.column [ Column.Width (Screen.All, Column.Is1)  ]
              [ Heading.h5 [] [ str ("Hand: ") ] ]
            Column.column [ Column.Width (Screen.All, Column.Is5) ]
                [ Heading.h5 [] [ str ("Discard pile:") ] ] ] 
        Columns.columns []
          handAndDiscardView
        match model.CurrentPlayer with
        | None -> ()
        | Some player ->
          Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Left) ] ]
              [ Heading.h5 [] [ str (player.Name) ] ]
          Columns.columns []
            (viewCards model dispatch PlayArea.Row1 player.Cards.Row1)
          Columns.columns []
            (viewCards model dispatch PlayArea.Row2 player.Cards.Row2)
          Columns.columns []
              [ Column.column [ Column.Width (Screen.All, Column.Is1) ] 
                  [ Button.button [ Button.Color IsDanger 
                                    Button.OnClick (fun _ -> dispatch EndTurn) ]
                      [ str "End turn" ] ] ] ]
                                     
let view (model : Model) (dispatch : Msg -> unit) =
    div []
        [ Navbar.navbar [ Navbar.Color IsPrimary ]
            [ Navbar.Item.div [ ]
                [ Heading.h2 [ ]
                    [ str "Golf" ] ] ]

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
