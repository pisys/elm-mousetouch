import Html exposing (div, text, button)
import Html.Attributes exposing (class)
import Html.Events exposing (defaultOptions)
import MouseTouch exposing (on, onMultiple)
import MouseTouch.Eval exposing (onClick, swipeDown, swipeUp, swipeLeft, swipeRight)
import Time exposing (second)
import Json.Decode as Json

type Action = Increment | Decrement | NoOp

view address model =
      div 
        [ class "frame" ]
        [ div [ class "info" ]
            [ text "Tap or click the buttons or swipe left/right over the counter" ]
        , button ( onClick "TapTest.Increment" address Increment ) [ text "+" ]
        , div 
            ( class "counter" 
            :: ( onMultiple
                [ { eval = (swipeRight 50 50)
                  , options = defaultOptions
                  , pruneBelow = (second/2) 
                  , key = "TapTest.Swipe.Increment" 
                  , decoder = Json.value 
                  , toMessage = (\_ -> Signal.message address Increment )
                  }
                , { eval = (swipeLeft 50 50)
                  , options = defaultOptions
                  , pruneBelow = (second/2) 
                  , key = "TapTest.Swipe.Decrement" 
                  , decoder = Json.value 
                  , toMessage = (\_ -> Signal.message address Decrement )
                  }
                ]
               ) 
           )
           [ text (toString model) ]
        , button ( onClick "TapTest.Decrement" address Decrement ) [ text "-" ]
        ]


update action model =
  case action of
    Increment -> model + 1
    Decrement -> model - 1
    NoOp      -> model

actions = Signal.mailbox NoOp
    
main = Signal.map (view actions.address) 
       <| Signal.foldp update 0 actions.signal

