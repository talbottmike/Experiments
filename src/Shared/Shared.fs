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
type NewPlayerStatus = | Joined | Ready
type NewPlayer = { Id : Guid; Name : string; ClientId : Guid; Status : NewPlayerStatus; }
type NewGame = { Players : NewPlayer list; NewUserName : string }
type PlayerCards = { Row1 : Card list; Row2 : Card list; Hand : Card option; }
type Player = { Id : Guid; ClientId : Guid; Name : string; Cards : PlayerCards; FinalScore : int option; Score : int; }
type RunningGame = { DrawPile : Card list; DiscardPile : Card list; Players : Player list; MovingCard : Card option; CurrentPlayer : Player option; }
type ServerRunningGame = { UndrawnCards : Card list; Game : RunningGame; }
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

type SharedGameState =
  | NewGame of NewGame
  | Running of RunningGame
  | Finished of FinishedGame

type ServerMsg =
    | NewGameMsg of ServerNewGameMsg
    | RunningGameMsg of ServerRunningGameMsg
    | RecoverGame of SharedGameState

/// A type that specifies the messages sent to the client from the server on Elmish.Bridge
type ClientNewGameMsg =
    | SyncPlayers of NewPlayer list
    | PlayerAdded of NewPlayer
    | PlayerStatusUpdated of (Guid * NewPlayerStatus)
    | DealtGame of RunningGame

type ClientRunningGameMsg =
  | AbortedGame

type ClientMsg =
  | NewGameMsg of ClientNewGameMsg
  | RunningGameMsg of ClientRunningGameMsg
  | FinishedGame of FinishedGame
  | GameState of SharedGameState

