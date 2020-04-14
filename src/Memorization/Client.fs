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
open System.Text.RegularExpressions

let verseReference = "1 Peter 1:3-5 English Standard Version (ESV)"
let verseText = "3 Blessed be the God and Father of our Lord Jesus Christ! According to his great mercy, he has caused us to be born again to a living hope through the resurrection of Jesus Christ from the dead, 4 to an inheritance that is imperishable, undefiled, and unfading, kept in heaven for you, 5 who by God's power are being guarded through faith for a salvation ready to be revealed in the last time."
type TextType = | Word | Punctuation | Number
type TextPart = { Text : string; TextType : TextType }
type Model = { Title : string; Text : string; TextParts : TextPart list } 

type Msg =
  | SetText of string
  
let (|FirstRegexGroup|_|) pattern input =
   let m = Regex.Match(input,pattern) 
   if (m.Success) then Some m.Groups.[1].Value else None  

let identifyTextType (s:string) =
  match s with
  | FirstRegexGroup "[\d]" v -> { Text = s; TextType = TextType.Number}
  | FirstRegexGroup "[^\w\s']" v -> { Text = s; TextType = TextType.Punctuation}
  | _ -> { Text = s; TextType = TextType.Word}
  
let getTextParts (t : string) = 
  Regex.Split(t, @"(\b[^\s]+\b)")
  |> Seq.map (fun x -> x.Trim())
  |> Seq.filter (fun x -> x <> "")
  |> Seq.map identifyTextType |> Seq.toList 
  
let init () : Model * Cmd<Msg> =
  let textParts = getTextParts verseText
  let model = { Title = verseReference; Text = verseText; TextParts = textParts; }
  model, Cmd.none
  
let update (msg : Msg) (model : Model) : Model * Cmd<Msg> =
  match msg with
  | SetText t ->
    let tp = getTextParts t
    { model with Text = t; TextParts = tp; }, Cmd.none
        
let [<Literal>] ESC_KEY = 27.
let [<Literal>] ENTER_KEY = 13.
let internal onEnter msg dispatch =
    function
    | (ev:Browser.Types.KeyboardEvent) when ev.keyCode = ENTER_KEY ->
        ev.target?value <- ""
        dispatch msg
    | _ -> ()
    |> OnKeyDown
                                     
let view (model : Model) (dispatch : Msg -> unit) =
    div []
        [ Navbar.navbar [ Navbar.Color IsPrimary ]
            [ Navbar.Item.div [ ]
                [ Heading.h2 [ ]
                    [ str "Memorization" ] ] ]
        
          Container.container []
              [ str model.Text
                Card.card []
                  [ Card.header []
                      [ h2 []
                          [ str model.Title ] ]
                    Card.content []
                      [ for x in model.TextParts do
                        match x.TextType with
                        | TextType.Word ->
                          Button.button [] [ str x.Text ]
                        | TextType.Punctuation ->
                          Button.button [ Button.Disabled true; Button.Color IsPrimary; ] [ str x.Text ]
                        | TextType.Number ->
                          Button.button [ Button.Disabled true; Button.Color IsInfo; ] [ str x.Text ] ] ] ] ]

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
