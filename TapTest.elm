import Html exposing (div, text, button)
import TapBox exposing (TapBox, tapbox, start, on)
import Time exposing (second)

type Action = Increment | Decrement | NoOp

myTapBox : TapBox Action
myTapBox = tapbox NoOp second

view address model =
  div [] 
    [ button ( on start address Decrement ) [ text "-" ]
    , div [] [ text (toString model) ]
    , button ( on start address Increment ) [ text "+" ]
    ]

update action model =
  case action of
    Increment -> model + 1
    Decrement -> model - 1
    
main = Signal.map (view myTapBox.address) 
       <| Signal.foldp update 0 myTapBox.signal

