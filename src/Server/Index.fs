module Index
open Giraffe.GiraffeViewEngine
open Fulma

let index =
  [ section [ _class "hero is-medium is-bold is-primary"; ]
            [ div [ _class "tile box" ]
                [ h2 [ ]
                    [ a [ _href "app" ] [ rawText "Golf Multiplayer" ] ] ]
              div [ _class "tile box" ]
                [ h2 [ ]
                    [ a [ _href "app/golf" ] [ rawText "Golf" ] ] ]
              div [ _class "tile box" ]
                [ h2 [ ]
                    [ a [ _href "app/tens" ] [ rawText "Tens" ] ] ] ] ]

let layout = Layout.layout index