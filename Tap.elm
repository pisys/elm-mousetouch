module Tap (onClick, onClickWithOptions, tapSignal) where
import Html exposing (Attribute, text, Html, div)
import Html.Events exposing (onWithOptions, Options)
import Json.Decode as Json
import Debug

onClickWithOptions : Options -> Signal.Address a -> a -> List Attribute
onClickWithOptions options address msg =
    let helper event a = 
        onWithOptions event options Json.value (\_ -> Signal.message touches.address a)
    in
    [helper "touchstart" (Start msg)
    ,helper "touchmove" (Cancel msg)
    ,helper "touchleave" (Cancel msg)
    ,helper "touchend" (End msg)
    ]

onClick = onClickWithOptions {stopPropagation = False, preventDefault = False}

touches : Signal.Mailbox (Tap a)
touches =
    Signal.mailbox NoTap

init : a -> TapModel a
init start = {
    noop = start,
    pending = start,
    carryOut = False
    }

type alias TapModel a = {
    noop : a,
    pending : a,
    carryOut : Bool
    }


type Tap a = Start a | End a | Cancel a | NoTap

touchesToAction : Tap a -> TapModel a -> TapModel a
touchesToAction tap model =
    case (Debug.log "tap" tap) of 
        NoTap -> model
        Start act -> 
            { model | pending <- act, carryOut <- False }
        End act ->
            if model.pending == act then 
                { model | carryOut <- True }
            else
                init model.noop
        Cancel act ->
            init model.noop

touchModel : a -> Signal (TapModel a)
touchModel a =
    Signal.foldp touchesToAction (init a) touches.signal

touchAction : TapModel a -> a
touchAction model =
    if model.carryOut then (Debug.log "touchAction" model.pending)
    else (Debug.log "touchAction" model.noop)

type Action = NoOp | Do Int

type alias Model = {
    one : Int,
    two : Int,
    three : Int
    } 

update: Action -> Model -> Model
update act model = 
    case act of
        NoOp -> model
        Do i -> 
            if | i == 1 -> {model| one <- model.one + 1}
               | i == 2 -> {model| two <- model.two + 1 }
               | i == 3 -> {model| three <- model.three + 1}
            

mailbox : Signal.Mailbox Action
mailbox = 
    Signal.mailbox NoOp

view : Signal.Address Action -> Model -> Html
view address model =
    div [] 
        [ div (onClick address (Do 1)) [text (toString model.one)]
        , div (onClick address (Do 2)) [text (toString model.two)]
        , div (onClick address (Do 3)) [text (toString model.three)]
        ]

tapSignal : a -> Signal a
tapSignal a =
    Signal.map touchAction (touchModel a)

model : Signal Model
model = Signal.merge mailbox.signal (tapSignal NoOp)
    |> Signal.foldp update {one=0,two=0,three=0}

main = 
    Signal.map (view mailbox.address) model

