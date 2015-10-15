module TapBox 
    ( PastEvents, Event, Device(..), Action(..), EvalFunction
    , on, onWithOptions, onClick
    , click', click, start', start
    ) 
    where

{-| TapBox wraps "low-level" mouse/touch events on HTML Elements and transforms
them into "high-level" events (eg. a click), hiding the user input method of 
the device (mouse, touch or both) and circumventing quirks like the 300ms delay 
of 'click' events on touch devices.

Low-level events are mousestart, mouseend, mousemove, mouseout, touchstart,
touchend, touchmove and touchleave.

High-level events are defined as evaluation functions which evaluate past 
low-level events.

# Definition
@docs PastEvents, Event, Device, Action, EvalFunction

# Listeners 
@docs on, onWithOptions, onClick

# Evaluation Functions
@docs click', click, start, start'

-}

import Html exposing (Attribute)
import Html.Events exposing (onWithOptions, Options, defaultOptions)
import Time exposing (Time, millisecond)
import Signal exposing (Address)
import Debug
import Json.Decode as Json
import Native.TapBox

{-| Devices.
-}
type Device = Mouse 
            | Touch

{-| Actions.
-}
type Action = Start 
            | End 
            | Leave 
            | Move

{-| Normalized low-level events. This type could be extended to keep coordinates as well.
-}
type alias Event = ((Device, Action), Time)

{-| The past events are a list of `Event`s. These need to be evaluated by an `EvalFunction`.
-}
type alias PastEvents = List Event

{-| The evaluation function evaluates past events.
-}
type alias EvalFunction = (PastEvents -> Bool)

messageOn : EvalFunction -> Signal.Address a -> a -> List Attribute
messageOn eval addr msg =
  on eval Json.value (\_ -> Signal.message addr msg)

{-|-}
onClick : Signal.Address a -> a -> List Attribute
onClick =
  messageOn click

{-| Event handler function which takes an evaluation function, options, 
an Address, a message and returns a list of `Attributes`.
Equivalent of `Html.Events.onWithOptions`.
-}
onWithOptions : EvalFunction
                -> Options
                -> Json.Decoder a
                -> (a -> Signal.Message)
                -> List Attribute
onWithOptions =
    Native.TapBox.on

{-| Event handler function which takes an evaluation function, 
an Address of Tapbox, a message and returns a list of `Attributes`.
Equivalent of `Html.Events.on`.
-}
on : EvalFunction 
     -> Json.Decoder a
     -> (a -> Signal.Message)
     -> List Attribute
on eval decoder toMessage =
    Native.TapBox.on eval defaultOptions decoder toMessage
        

{-| A click evaluation function which has to be parametrized with the maximum range
between the start and end event (ie. mousedown/mouseup or touchstart/touchend).

Click searches past events for a pattern of mouseend (mousemove{0,5}) mousestart
or touchend (touchmove|mouseevent){0|5} touchstart. The latter ignores virtual 
mouse events intermingling with a fast series of touches.
-}

click' : Time -> Time -> PastEvents -> Bool
click' maxRange minLast events =
    let
        diffTouch = 
            case Debug.log "events" events of
                ((Touch, End), time1) 
                :: rest -> 
                    case List.filter 
                            (\((d,a),_) -> d == Touch && a /= Move) 
                            rest of
                        ((Touch, Start), time2) 
                        :: rest ->
                            Just <| time1 - time2
                        _ -> Nothing
                _ -> Nothing

        (diffMouse,lastTouch) =
            case events of
                ((Mouse, End), time1) 
                :: rest ->
                    case List.filter 
                            (\((d,a),_) -> d == Mouse && a /= Move) 
                            rest of
                        ((Mouse, Start), time2) 
                        :: rest -> 
                            ( Just <| time1 - time2,
                                case List.filter
                                        (\((d,a),_) -> d == Touch)
                                        events of
                                    ((Touch, _), last) 
                                    :: rest ->
                                        Just <| time2 - last
                                    _ ->
                                        Nothing
                            )
                        _ -> 
                            (Nothing,Nothing)
                _ -> 
                    (Nothing,Nothing)

    in
       case diffTouch of
           Nothing -> 
               case Debug.log "diffMouse" (diffMouse,lastTouch) of
                   (Just d, Nothing) -> 
                       d < maxRange
                   (Just d, Just l) ->
                       d < maxRange && l > minLast
                   _ -> False
           Just d ->
               d < maxRange

{-| A convenient click evaluation function in the sense of start and end happening 
within 300 ms and not past virtual mouse event must exist within the last 700 ms
-}
click : EvalFunction
click = 
    click' (300*millisecond) (700*millisecond)

{-| A simple mousestart/touchstart evaluation function. Takes the minimum time distance 
to the last mouse event. 
-}
start' : Time -> PastEvents -> Bool
start' minLast events =
    let 
        (time, last) =
            case events of
                ((d, Start), time) 
                :: rest ->
                    (Just time, 
                        if d == Mouse 
                           then 
                               case List.filter 
                                        (\((d,a),_) -> d == Touch) 
                                        rest of
                                   ((Touch,_),last) 
                                   :: rest ->
                                       Just last
                                   _ -> Nothing
                           else 
                               Nothing
                   )
                _ -> (Nothing, Nothing)

    in
       case time of
           Nothing -> False
           Just t -> 
               case Maybe.map ((-) t) last of 
                   Nothing -> True
                   Just l -> l > minLast

{-| A wrapper around `start'` configured with 700 milliseconds distance to last 
mouse event.
-}
start : EvalFunction
start =
    start' (700*millisecond) 
