import Html exposing (div, text, button)
import TapBox exposing (click, on)
import Time exposing (second)

type Action = Increment | Decrement | NoOp

view address model =
  div [] 
    [ button ( on click address Decrement ) [ text "-" ]
    , div [] [ text (toString model) ]
    , button ( on click address Increment ) [ text "+" ]
    ]


update action model =
  case action of
    Increment -> model + 1
    Decrement -> model - 1

actions = Signal.mailbox NoOp
    
main = Signal.map (view actions.address) 
       <| Signal.foldp update 0 actions.signal

