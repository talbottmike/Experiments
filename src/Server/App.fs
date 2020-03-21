module App
open Giraffe.GiraffeViewEngine

let index =
  [ yield div [ _id "elmish-app" ] []
    yield script [ _src (HashInfo.withHash "/resources/vendors" "js") ] []
    yield script [ _src (HashInfo.withHash "/resources/app" "js") ] [] ]

let layout = Layout.layout index