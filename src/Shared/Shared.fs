namespace Shared
open System

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
    | King -> 0
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
type NewPlayerStatus = | Joined | Ready
type NewPlayer = { Id : Guid; Name : string; ClientId : Guid; Status : NewPlayerStatus; }
type NewGame = { Players : NewPlayer list; NewUserName : string }
type PlayerCards = { Row1 : Card list; Row2 : Card list; Hand : Card option; }
type Player = { Id : Guid; ClientId : Guid; Name : string; Cards : PlayerCards; FinalScore : int option; Score : int; }
type ClientRunningGame = { DrawPile : Card list; DiscardPile : Card list; Players : Player list; MovingCard : Card option; CurrentPlayer : Player option; }
type ServerRunningGame = { UndrawnCards : Card list; DrawPile : Card list; DiscardPile : Card list; Players : Player list; MovingCard : Card option; CurrentPlayer : Player option; }

type FinishedGame = { Players : Player list; }

type RunningGameType =
  | ServerType of ServerRunningGame
  | ClientType of ClientRunningGame

type GameState =
  | NewGame of NewGame
  | Running of RunningGameType
  | Finished of FinishedGame

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

 /// A type that specifies the messages sent to the server from the client on Elmish.Bridge
/// to learn more, read about at https://github.com/Nhowka/Elmish.Bridge#shared
type ServerNewGameMsg =
    | AddPlayer of NewPlayer
    | SetPlayerStatus of (Guid * NewPlayerStatus)
    | GetExistingPlayers
    | Deal

type ServerRunningGameMsg =
    | DrawCard
    | FlipCard of Card
    | CancelMove
    | Move of Card
    | DropOn of DropDestination
    | EndTurn
    | GameOver
    | AbortGame
    | RequestState

type ServerMsg =
    | NewGameMsg of ServerNewGameMsg
    | RunningGameMsg of ServerRunningGameMsg
    | RecoverGame of GameState
    | QuitGame of Guid

/// A type that specifies the messages sent to the client from the server on Elmish.Bridge
type ClientNewGameMsg =
  | SyncPlayers of NewPlayer list
  | PlayerAdded of NewPlayer
  | PlayerStatusUpdated of (Guid * NewPlayerStatus)

type ClientRunningGameMsg =
  | AbortedGame

type ClientMsg =
  | NewGameMsg of ClientNewGameMsg
  | RunningGameMsg of ClientRunningGameMsg
  | FinishedGame of FinishedGame
  | GameState of GameState
  | DealtGame of ClientRunningGame

module Golf =
  let blankGame = (NewGame { Players = []; NewUserName = ""; })

  let getClassName (card : Card) = 
    let suit, rank = card.Value
    let s = sprintf "%A" suit
    let r =
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
    sprintf "css-sprite-Card-%s%s" r s
  
  let addPlayer (p : NewPlayer) = 
    { Player.Id = p.Id
      ClientId = p.ClientId
      Name = p.Name
      Cards = { Row1 = []; Row2 = []; Hand = None;}
      FinalScore = None; Score = 0; }
    
  let shuffle cards = cards |> List.sortBy (fun _ -> Guid.NewGuid())
    
  let singleDeckOfCards =
    CardRank.allValues |> List.collect (fun r -> CardSuit.allValues |> List.map (fun s -> (s,r)))

  let deckOfCards =
    [ 1..4 ] 
    |> List.collect (fun _ -> singleDeckOfCards)
    |> List.map (fun v -> { Id = Guid.NewGuid(); Value = v; Position = FaceDown; Selected = false; })

  let dealPlayer (player : Player) (undealtCards : Card list) =
    let playerRow1Cards, playerRow2Cards, remainingCards = undealtCards.[..5], undealtCards.[6..11], undealtCards.[12..]
    let playerCards = { Row1 = playerRow1Cards; Row2 = playerRow2Cards; Hand = None; }
    ({ player with Cards = playerCards; FinalScore = None; }, remainingCards)

  let deal (players : Player list) = 
    let shuffledCards = shuffle deckOfCards
    let playersWithNewCards, undealtCards =
      players
      |> List.fold 
        (fun (playersDealt, accumulatedCards : Card list) (player : Player) ->
            let player, remainingCards = dealPlayer player accumulatedCards
            (player :: playersDealt, remainingCards)
        ) 
        (List.empty<Player>,shuffledCards)

    let discardPile, drawPile, serverDrawPile = [{ undealtCards.Head with Position = FaceUp }], undealtCards.Tail.[..3], undealtCards.Tail.[4..]

    let sortedPlayersWithNewCards = playersWithNewCards |> List.sortBy (fun x -> x.Id)
    { UndrawnCards = serverDrawPile; DrawPile = drawPile ; DiscardPile = discardPile; Players = sortedPlayersWithNewCards; MovingCard = None; CurrentPlayer = sortedPlayersWithNewCards |> List.tryHead; }

  let serverGameToClientGame (g : ServerRunningGame) =
    { DrawPile = g.DrawPile; DiscardPile = g.DiscardPile; Players = g.Players; MovingCard = g.MovingCard; CurrentPlayer = g.CurrentPlayer; }

  let clientGameToServerGame (g : ClientRunningGame) =
    let shuffledCards = shuffle deckOfCards
    let undrawnCards = shuffledCards 
    { UndrawnCards = undrawnCards; DrawPile = g.DrawPile; DiscardPile = g.DiscardPile; Players = g.Players; MovingCard = g.MovingCard; CurrentPlayer = g.CurrentPlayer; }

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


  let flipUp (card : Card) = { card with Position = FaceUp; }
  let flipDown (card : Card) = { card with Position = FaceDown; }

  let scoreCards cards =
    let pairedCards = List.zip cards.Row1 cards.Row2
    let individualColumnResults =
      pairedCards
      |> List.sumBy (fun (r1,r2) -> 
        match r1.Value, r2.Value with
        | (_,Joker),(_,Joker) -> (Joker.toInt()) + (Joker.toInt())
        | (_,a),(_,b) when a = b -> 0
        | (_,a),(_,b) -> (a.toInt()) + (b.toInt())
      )

    let pairedColumnResults =
      pairedCards
      |> List.windowed 2
      |> List.sumBy (fun window -> 
          let cardRanks = window |> List.collect (fun (x,y) -> [x.Value;y.Value;]) |> List.map snd
          //[a.Value; b.Value; d.Value; e.Value] |> List.map snd
          match cardRanks.Head, cardRanks with
          | Joker, cards when cards |> List.forall (fun x -> x = Joker) -> -20
          | v, cards when cards |> List.forall (fun x -> x = v) -> -10
          | _ -> 0
      )

    individualColumnResults + pairedColumnResults

  let scorePlayer player =
    let score = scoreCards player.Cards
    { player with FinalScore = Some score; Score = player.Score + score }

  let removeCard cards card = cards |> List.filter (fun c -> c <> card)
  let replaceCard cards newCard oldCard = cards |> List.map (fun c -> if c = oldCard then newCard else c) 
  // let tryFlip cards card = cards |> List.map (fun c -> if c = card then flip c else c )
  let tryFlipUp cards card = cards |> List.map (fun c -> if c = card then flipUp c else c )
      
  let endTurn (game : ServerRunningGame) =
    let newGameState =
        let playersUpdatedFromPreviousTurn = game.CurrentPlayer |> Option.map (fun player -> game.Players |> List.map (fun x -> if x.Id = player.Id then player else x) |> List.sortBy (fun x -> x.Id)) |> Option.defaultValue game.Players
        let newCurrentPlayer = game.CurrentPlayer |> Option.bind (fun player -> game.Players |> List.sortBy (fun x -> x.Id) |> List.tryFind (fun x -> x.Id > player.Id)) |> Option.orElse (playersUpdatedFromPreviousTurn |> List.tryHead)
        { game with Players = playersUpdatedFromPreviousTurn; CurrentPlayer = newCurrentPlayer; }
        
    newGameState

  let dropCard (model : ServerRunningGame) player dropDestination =
    model.MovingCard
    |> Option.map (fun sourceCard -> 
      getCardPlayArea model sourceCard
      |> Option.map (fun (sourceArea) ->
        match sourceArea, dropDestination with
        | PlayArea.DrawPile, DiscardPile ->
          let replacementCard, serverDrawPile = model.UndrawnCards.Head, model.UndrawnCards.Tail
          { model with
              UndrawnCards = serverDrawPile
              DrawPile = replaceCard model.DrawPile replacementCard sourceCard
              DiscardPile = sourceCard :: (model.DiscardPile |> List.map flipDown)
              MovingCard = None }, true
        | PlayArea.DrawPile, Row1 destinationCard ->
          let replacementCard, serverDrawPile = model.UndrawnCards.Head, model.UndrawnCards.Tail
          { model with
              UndrawnCards = serverDrawPile
              CurrentPlayer = Some { player with Cards = { player.Cards with Row1 = replaceCard player.Cards.Row1 sourceCard destinationCard; Hand = Some (flipUp destinationCard); } }
              DrawPile = replaceCard model.DrawPile replacementCard sourceCard
              MovingCard = None }, false
        | PlayArea.DrawPile, Row2 destinationCard ->
          let replacementCard, serverDrawPile = model.UndrawnCards.Head, model.UndrawnCards.Tail
          { model with
              UndrawnCards = serverDrawPile 
              CurrentPlayer = Some { player with Cards = { player.Cards with Row2 = replaceCard player.Cards.Row2 sourceCard destinationCard; Hand = Some (flipUp destinationCard); } }
              DrawPile = replaceCard model.DrawPile replacementCard sourceCard
              MovingCard = None }, false
        | PlayArea.Hand, DiscardPile ->
          { model with
              CurrentPlayer = Some { player with Cards = { player.Cards with Hand = None; } }
              DiscardPile = sourceCard :: (model.DiscardPile |> List.map flipDown)
              MovingCard = None }, true
        | PlayArea.DiscardPile, Row1 destinationCard ->
          { model with
              CurrentPlayer = Some { player with Cards = { player.Cards with Row1 = replaceCard player.Cards.Row1 sourceCard destinationCard; Hand = Some (flipUp destinationCard); } }
              DiscardPile = model.DiscardPile.Tail |> List.mapi (fun i x -> if i = 0 then flipUp x else x)
              MovingCard = None }, false
        | PlayArea.DiscardPile, Row2 destinationCard ->
          { model with
              CurrentPlayer = Some { player with Cards = { player.Cards with Row2 = replaceCard player.Cards.Row2 sourceCard destinationCard; Hand = Some (flipUp destinationCard); } }
              DiscardPile = model.DiscardPile.Tail |> List.mapi (fun i x -> if i = 0 then flipUp x else x)
              MovingCard = None }, false
        | PlayArea.Hand, Row1 destinationCard ->
          { model with
              CurrentPlayer = Some { player with Cards = { player.Cards with Row1 = replaceCard player.Cards.Row1 sourceCard destinationCard; Hand = Some (flipUp destinationCard); } }
              MovingCard = None }, false
        | PlayArea.Hand, Row2 destinationCard ->
          { model with
              CurrentPlayer = Some { player with Cards = { player.Cards with Row2 = replaceCard player.Cards.Row2 sourceCard destinationCard; Hand = Some (flipUp destinationCard); } }
              MovingCard = None }, false
        | _ -> model, false
      )
      |> Option.defaultValue (model, false)
    )
    |> Option.defaultValue (model, false)

  let endGame (players : Player list) =
    let finishedGame = { FinishedGame.Players = players |> List.map scorePlayer }
    let newModel = Finished finishedGame
    newModel, [(finishedGame |> ClientMsg.FinishedGame)]

  let handleRunningUpdate (model : ServerRunningGame) msg =
    let isGameOngoing = model.Players |> List.exists (fun x -> x.Cards.Row1 |> List.exists (fun c -> c.Position = FaceDown) || x.Cards.Row2 |> List.exists (fun c -> c.Position = FaceDown))
    match model.CurrentPlayer, msg with
    | _ when (not isGameOngoing) ->
      endGame model.Players
    | _ , RequestState ->
      let clientGame = serverGameToClientGame model
      Running (ServerType model), [(clientGame |> ClientType |> Running |> ClientMsg.GameState)]
    | None, _ -> 
      endGame model.Players
    | _, EndTurn ->
      let newGameState = endTurn model
      let clientGame = serverGameToClientGame newGameState
      Running (ServerType newGameState), [(clientGame |> ClientType |> Running |> ClientMsg.GameState)]
    | Some player, FlipCard card ->
      let newRunningState = 
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
      Running (ServerType newRunningState), [(serverGameToClientGame newRunningState |> ClientType |> Running |> ClientMsg.GameState)]
    | _, Move selectedCard ->
      let newRunningState = { model with MovingCard = Some selectedCard }
      Running (ServerType newRunningState), [(serverGameToClientGame newRunningState |> ClientType |> Running |> ClientMsg.GameState)]
    | _, CancelMove ->
      let newRunningState = {model with MovingCard = None}
      Running (ServerType newRunningState), [(serverGameToClientGame newRunningState |> ClientType |> Running |> ClientMsg.GameState)]
    | Some player, DropOn dropDestination ->
      let newRunningState, shouldEndTurn = dropCard model player dropDestination
      let finalGameState =
        if shouldEndTurn then
          endTurn newRunningState
        else 
          newRunningState
      Running (ServerType finalGameState), [(serverGameToClientGame finalGameState |> ClientType |> Running |> ClientMsg.GameState)]
    | _, GameOver ->
      let finishedGame = { FinishedGame.Players = model.Players |> List.map scorePlayer }
      let newModel = Finished finishedGame
      
      newModel, [(finishedGame |> ClientMsg.FinishedGame)]
    | _ -> Running (ServerType model), []
  let handleUpdate (currentModel : GameState) msg  =
    try
      printfn "Current model: %A" currentModel
      match currentModel, msg with
      | _, ServerMsg.RunningGameMsg AbortGame ->            
        blankGame, [(ClientMsg.RunningGameMsg AbortedGame)]
      | NewGame model, ServerMsg.RecoverGame gameState when model.Players.Length = 0 ->
        let newModel =
          match gameState with
          | Running runningGameType ->
            match runningGameType with
            | ClientType clientRunningGame -> 
              let recoveredGame = clientGameToServerGame clientRunningGame |> RunningGameType.ServerType
              Running recoveredGame
            | v -> Running v
          | v -> v
        newModel, []
      | NewGame model, ServerMsg.QuitGame playerId ->
        let newPlayers = model.Players |> List.filter (fun x -> x.Id <> playerId)
        let newModel = { model with Players = newPlayers }
        let newMsg =  newModel.Players |> ClientNewGameMsg.SyncPlayers |> ClientMsg.NewGameMsg 
        NewGame newModel, [ newMsg ]
      | NewGame model, ServerMsg.NewGameMsg (AddPlayer newPlayer) ->
        let newModel = { model with Players = newPlayer :: model.Players }
        printfn "New model after add player: %A" newModel
        // if it is a new client, send them everyone
        let newMsg =
          if model.Players |> List.exists (fun x -> x.ClientId = newPlayer.ClientId)
          then
            (PlayerAdded newPlayer |> ClientMsg.NewGameMsg)
          else
            (SyncPlayers newModel.Players |> ClientMsg.NewGameMsg)
        NewGame newModel, [ newMsg ]
      | NewGame model, ServerMsg.NewGameMsg GetExistingPlayers ->
        currentModel, [(SyncPlayers model.Players |> ClientMsg.NewGameMsg)]
      | NewGame model, ServerMsg.NewGameMsg (SetPlayerStatus (playerId, newStatus)) ->
        let updatedPlayers = model.Players |> List.map (fun x -> if x.Id = playerId then { x with Status = newStatus} else x)
        if updatedPlayers |> List.forall (fun x -> x.Status = NewPlayerStatus.Ready) |> not then
          let newModel = { model with Players = updatedPlayers }
          NewGame newModel, [(PlayerStatusUpdated (playerId, newStatus) |> ClientMsg.NewGameMsg)]
        else
          let runningGame = updatedPlayers |> List.map addPlayer |> deal
          let clientGame = serverGameToClientGame runningGame
          Running (ServerType runningGame), [(clientGame |> ClientMsg.DealtGame)]
      | NewGame model, ServerMsg.NewGameMsg Deal when model.Players.Length > 0 ->
        let runningGame = model.Players |> List.map addPlayer |> deal
        let clientGame = serverGameToClientGame runningGame
        Running (ServerType runningGame), [(clientGame |> ClientMsg.DealtGame)]
      | NewGame model , ServerMsg.RunningGameMsg RequestState ->
        currentModel, [(AbortedGame |> ClientMsg.RunningGameMsg)]
      | Running (ServerType model), ServerMsg.RecoverGame _ ->
        let clientGame = serverGameToClientGame model
        Running (ServerType model), [(clientGame |> ClientType |> Running |> ClientMsg.GameState)]
      | Running (ServerType model), ServerMsg.QuitGame playerId ->
        match model.Players |> List.filter (fun x -> x.Id <> playerId) with
        | [] -> 
          blankGame, [(ClientMsg.RunningGameMsg AbortedGame)]
        | newPlayers ->
          let intermediateState = { model with Players = newPlayers }
          let newGameState =
            match model.CurrentPlayer with
            | Some currentPlayer when currentPlayer.Id = playerId -> endTurn intermediateState
            | _ -> intermediateState
          let clientGame = serverGameToClientGame newGameState
          Running (ServerType newGameState), [(clientGame |> ClientType |> Running |> ClientMsg.GameState)]
      | Running (ServerType model), ServerMsg.RunningGameMsg msg ->
        handleRunningUpdate model msg
      | Finished model, ServerMsg.NewGameMsg Deal ->
        let runningGame = deal model.Players
        Running (ServerType runningGame), [(serverGameToClientGame runningGame |> ClientMsg.DealtGame)]
      | Finished model, ServerMsg.QuitGame playerId ->
        match model.Players |> List.filter (fun x -> x.Id <> playerId) with
        | [] -> 
          blankGame, [(ClientMsg.RunningGameMsg AbortedGame)]
        | newPlayers ->
          let newModel = { model with Players = newPlayers }
          let newMsg =  ClientMsg.FinishedGame newModel 
          Finished newModel, [ newMsg ]
      | Running (ServerType model), (ServerMsg.NewGameMsg (AddPlayer newPlayer)) ->
        let player, remainingCards = addPlayer newPlayer |> (fun x -> dealPlayer x model.UndrawnCards)
        let newModel = { model with Players = player :: model.Players; UndrawnCards = remainingCards }
        let clientGame = serverGameToClientGame model
        Running (ServerType newModel), [(clientGame |> ClientType |> Running |> ClientMsg.GameState)]
      | NewGame _, (ServerMsg.RunningGameMsg _)
      | Finished _, (ServerMsg.NewGameMsg _)
      | Finished _, (ServerMsg.RunningGameMsg _)
      | _ ->
        printfn "Unhandled msg: %A for state: %A" msg currentModel
        currentModel,[]

    with
    | ex -> // This is best effort. If it fails, oh well.
      currentModel,[]