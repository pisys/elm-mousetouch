import Html exposing (div, text, button)
import Html.Attributes exposing (class)
import MouseTouch exposing (on)
import MouseTouch.Eval exposing (onClick, swipeDown, swipeUp)
import Time exposing (second)
import Json.Decode as Json

type Action = Increment | Decrement | Reset | NoOp

view address model =
    div 
        (on 
            swipeDown 
            (second/2) 
            "TapTest.Swipe.Decrement" 
            Json.value 
            (\_ -> Signal.message address Decrement )
        )
        [
          div 
            ( class "frame"
            :: ( on 
                    swipeUp 
                    (second/2) 
                    "TapTest.Swipe.Increment" 
                    Json.value 
                    (\_ -> Signal.message address Increment )
               )
            )
            [ button ( onClick "TapTest.Decrement" address Decrement ) [ text "-" ]
            , div [ class "counter" ] [ text (toString model) ]
            , button ( onClick "TapTest.Increment" address Increment ) [ text "+" ]
            ]
        ]


update action model =
  case action of
    Increment -> model + 1
    Decrement -> model - 1
    Reset -> 0

actions = Signal.mailbox NoOp
    
main = Signal.map (view actions.address) 
       <| Signal.foldp update 0 actions.signal

