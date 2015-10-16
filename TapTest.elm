import Html exposing (div, text, button)
import Html.Attributes exposing (class)
import Html.Events exposing (defaultOptions)
import MouseTouch exposing (on, onMultiple)
import MouseTouch.Eval exposing (onClick, swipeDown, swipeUp)
import Time exposing (second)
import Json.Decode as Json

type Action = Increment | Decrement | Reset | NoOp

view address model =
      div 
        ( class "frame"
        :: ( onMultiple
                [ { eval = swipeUp 
                  , options = defaultOptions
                  , pruneBelow = (second/2) 
                  , key = "TapTest.Swipe.Increment" 
                  , decoder = Json.value 
                  , toMessage = (\_ -> Signal.message address Increment )
                  }
                , { eval = swipeDown
                  , options = defaultOptions
                  , pruneBelow = (second/2) 
                  , key = "TapTest.Swipe.Decrement" 
                  , decoder = Json.value 
                  , toMessage = (\_ -> Signal.message address Decrement )
                  }
                ]
           )
        )
        [ button [] {-( onClick "TapTest.Decrement" address Decrement )-} [ text "-" ]
        , div [ class "counter" ] [ text (toString model) ]
        , button [] {-( onClick "TapTest.Increment" address Increment )-} [ text "+" ]
        ]


update action model =
  case action of
    Increment -> model + 1
    Decrement -> model - 1
    Reset -> 0

actions = Signal.mailbox NoOp
    
main = Signal.map (view actions.address) 
       <| Signal.foldp update 0 actions.signal

