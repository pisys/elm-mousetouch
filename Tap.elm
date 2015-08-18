module Tap (TapBox,tapbox) where
import Html exposing (Attribute, text, Html, div)
import Html.Events 
import Json.Decode as Json
import Debug

type alias TapBox a = {
        on: ((TapModel a -> Bool) -> a -> List Attribute),
        onWithOptions : (Html.Events.Options -> (TapModel a -> Bool) -> a -> List Attribute),
        signal : Signal a
    }

tapbox : a -> TapBox a
tapbox noop = 
    let mailbox = touches noop

        onWithOptions options hle msg =
            let 
                helper event x = 
                    Html.Events.onWithOptions event options Json.value (\_ -> Signal.message 
                        (Signal.forwardTo mailbox.address Just) 
                    x)
            in
                [helper "touchstart" (((Touch, Start), hle), msg)
                ,helper "touchmove" (((Touch, Move), hle), msg)
                ,helper "touchleave" (((Touch, Leave), hle), msg)
                ,helper "touchend" (((Touch, End), hle), msg)
                ,helper "mousedown" (((Mouse, Start), hle), msg)
                ,helper "mousemove" (((Mouse, Move), hle), msg)
                ,helper "mouseout" (((Mouse, Leave), hle), msg)
                ,helper "mouseup" (((Mouse, End), hle), msg)
                ]

    in 
        {
            onWithOptions = onWithOptions,
            on = onWithOptions {stopPropagation = False, preventDefault = False},
            signal = 
                Signal.filterMap parseAction noop 
                    <| Signal.map parseInput
                    <| Signal.foldp updateModel init mailbox.signal
        }

type Device = Mouse | Touch
type Action = Start | End | Leave | Move

touches : a -> Signal.Mailbox (Maybe (((Device,Action), (TapModel a -> Bool)), a))
touches a =
    Signal.mailbox Nothing

init : TapModel a
init = {
    pending = Nothing,
    carryOut = False
    }

type alias TapModel a = {
    pending : Maybe a,
    carryOut : Bool
    }

updateModel : Maybe (((Device, Action), (TapModel a -> Bool)), a) -> TapModel a -> TapModel a
updateModel tap model =
    model

parseInput : TapModel a -> TapModel a
parseInput model = 
    model

parseAction : TapModel a -> Maybe a
parseAction model =
    model.pending

