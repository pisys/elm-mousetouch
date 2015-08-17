module Tap (TapBox,tapbox) where
import Html exposing (Attribute, text, Html, div)
import Html.Events exposing (onWithOptions, Options)
import Json.Decode as Json
import Debug

type alias TapBox a = {
        onClick : (a -> List Attribute),
        onClickWithOptions : (Options -> a -> List Attribute),
        signal : Signal a
    }

tapbox : a -> TapBox a
tapbox noop = 
    let mailbox = touches noop

        onClickWithOptions options msg =
            let 
                helper event x = 
                    onWithOptions event options Json.value (\_ -> Signal.message mailbox.address x)
            in
                [helper "mousedown" (Start msg)
                ,helper "mousemove" (Cancel msg)
                ,helper "touchleave" (Cancel msg)
                ,helper "mouseup" (End msg)
                ]

    in 
        {
            onClickWithOptions = onClickWithOptions,
            onClick = onClickWithOptions {stopPropagation = False, preventDefault = False},
            signal = Signal.filterMap touchAction noop (Signal.foldp touchesToAction init mailbox.signal)
        }


touches : a -> Signal.Mailbox (Tap a)
touches a =
    Signal.mailbox NoTap

init : TapModel a
init = {
    pending = Nothing,
    carryOut = False
    }

type alias TapModel a = {
    pending : Maybe a,
    carryOut : Bool
    }


type Tap a = Start a | End a | Cancel a | NoTap

touchesToAction : Tap a -> TapModel a -> TapModel a
touchesToAction tap model =
    case (Debug.log "tap" tap) of 
        NoTap -> model
        Start act -> 
            { model | pending <- Just act, carryOut <- False }
        End act ->
            if model.pending == Just act then 
                { model | carryOut <- True }
            else
                init 
        Cancel act ->
            init 

touchAction : TapModel a -> Maybe a
touchAction model =
    if model.carryOut then (Debug.log "touchAction" model.pending)
    else (Debug.log "touchAction" Nothing)

