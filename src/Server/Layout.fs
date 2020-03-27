module Layout
open Giraffe.GiraffeViewEngine

let headLayout (content : XmlNode list) =
  head [] [ meta [ _charset "utf-8" ]
            meta [ _name "viewport"
                   _content "width=device-width, initial-scale=1" ]
            title [] [ encodedText "Games" ]
            link [ _rel "stylesheet"
                   _href "/resources/style.css" ]
            link [ _rel "stylesheet"

                   _href
                     "https://use.fontawesome.com/releases/v5.6.1/css/all.css"

                   _integrity
                     "sha384-gfdkjb5BdAXd+lj+gudLWI+BXq4IuLW5IT+brZEZsLFm++aCMlF1V92rMkPaX4PP"
                   _crossorigin "anonymous" ] ]

// link [_rel "shortcut icon"; _type "image/png"; _href "/assets/merge_arrow_black.png" ]
let headBody (content : XmlNode list) = body [] [ yield! content ]

let layout (content : XmlNode list) =
  html [ _class "has-background-grey-lighter" ] 
    [ yield headLayout content
      yield headBody content ]