import Html exposing (div, text, button)
import TapBox exposing (onClick)
import Time exposing (second)
import Json.Decode as Json

type Action = Increment | Decrement | NoOp

view address model =
  div [] 
    [ button ( onClick address Decrement ) [ text "-" ]
    , div [] [ text (toString model) ]
    , button ( onClick address Increment ) [ text "+" ]
    ]


update action model =
  case action of
    Increment -> model + 1
    Decrement -> model - 1

actions = Signal.mailbox NoOp
    
main = Signal.map (view actions.address) 
       <| Signal.foldp update 0 actions.signal

