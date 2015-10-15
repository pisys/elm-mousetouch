module TapBox 
    ( TapBox, PastEvents, Event, Device(..), Action(..), EvalFunction
    , tapbox, on, onWithOptions
    , click', click, start
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
@docs TapBox, PastEvents, Event, Device, Action, EvalFunction, tapbox

# Listeners 
@docs on, onWithOptions

# Evaluation Functions
@docs click', click, start

-}

import Html exposing (Attribute, text, Html, div)
import Html.Events exposing (onWithOptions, Options)
import Json.Decode as Json
import Time exposing (Time, millisecond, timestamp, second)
import Regex exposing (..)
import String
import Signal exposing (Address)
import Debug
import Native.TapBox

{-| TapModel represents the past of mouse/touch events. Stores them relative to
`timebase`. Associates an `EvalFunction` with a user defined value.
-}
type alias TapModel a =
    { pastEvents : PastEvents
    , eval : Maybe (EvalFunction, a)
    , timebase : Time
    }

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

{-| TapBox functions like a Signal.Mailbox. The difference is that it exposes
event handlers instead of an address. `signal` yields a Signal of user defined
type given the result of the `EvalFunction`.

Event handlers wrap Html.Events.onWithOptions.

    type Action = Increment | Decrement | NoOp

    myTapBox : TapBox Action
    myTapBox = tapbox NoOp second

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
        
    main = Signal.map (view myTapBox.address) 
           <| Signal.foldp update 0 myTapBox.signal
-}
type alias TapBox a = 
    { signal : Signal a
    , address : Signal.Address (Maybe (LowLevelHandler a))
    }

{-| The evaluation function evaluates past events.
-}
type alias EvalFunction = (PastEvents -> Bool)

{-| Combines a normalized low-level event with an `EvalFunction` and with a 
user-defined message which will be send on positive evaluation of `PastEvents`.
-}
type alias LowLevelHandler a = 
        { eval : EvalFunction
        , msg : a
        , device : Device
        , action : Action
        }

init : TapModel a
init = 
    { pastEvents = []
    , eval = Nothing
    , timebase = 0
    }

{-| Update the TapModel with the incoming event.
* Prune events older than `prune`
* Reset timebase and adjust past events after a time of `prune`
-}
update : Time 
    -> (Time, Maybe (LowLevelHandler a))
    -> TapModel a 
    -> TapModel a
update prune (t, e) model =
    case e of 
        Nothing -> model
        Just {device, action, eval, msg} -> 
            let 
                newTimebase = 
                    if (model.timebase + prune) < (t - prune)
                        then t - prune
                        else model.timebase
                newEventTime = 
                    t - newTimebase
                newEvent = 
                    ((device, action), newEventTime)
                pruneBelow = 
                    t - model.timebase - prune
                newPastEvents = 
                    adjustPastEvents (newTimebase - model.timebase) 
                    <| pruneEvents pruneBelow model.pastEvents
            in 
               { model | pastEvents <- newEvent :: newPastEvents
                       , eval <- Just (eval, msg)
                       , timebase <- newTimebase
               }

{-| Prune past events below `pruneBelow`.
-}
pruneEvents : Time -> PastEvents -> PastEvents
pruneEvents pruneBelow events =
    let 
        fold (e,t) list =
            if t < pruneBelow
               then list
               else (e,t) :: list
    in
        if pruneBelow <= 0
            then events
            else 
                List.foldr fold [] events

{-| Adjust past events to a new timebase.
-}
adjustPastEvents : Time -> PastEvents -> PastEvents
adjustPastEvents diffTimebase events =
    let
        adjustTime (e,t) =
            ( e, t - diffTimebase )
    in
        if diffTimebase == 0 
            then events
            else 
                List.map adjustTime events

{-| Apply the `eval` function to the past events.
-}
evaluate : TapModel a -> Maybe a
evaluate model =
    case model.eval of
        Nothing -> Nothing
        Just (evalFunction, msg) -> 
            if evalFunction model.pastEvents
               then Just msg
               else Nothing

-- SIGNALS

{-| A Mailbox wrapping the user defined action in low-level events.
-}
touches : a -> Signal.Mailbox (Maybe (LowLevelHandler a))
touches a =
    Signal.mailbox Nothing

{-| Construct a TapBox given a default value of user defined type and the time
events reside in the buffer before they are pruned. 

Creates an internal Signal.Mailbox and wires its address with mouse and touch
event handlers. The signal is folded into an internal model which represents the
past of the events within `prune` time. On any event the evaluation function is 
applied to the past events. 
-}
tapbox : a -> Time -> TapBox a
tapbox noop prune = 
    let tMailbox = touches noop

    in 
        { address = tMailbox.address
        , signal = 
            Signal.filterMap evaluate noop 
                <| Signal.foldp (update prune) init
                <| timestamp tMailbox.signal
        }

{-| Event handler function which takes an evaluation function, options, 
an Address of Tapbox, a message and returns a list of `Attributes`.
Equivalent of `Html.Events.onWithOptions`.
-}
onWithOptions : String
                -> EvalFunction
                -> Options
                -> Address a
                -> a 
                -> List Attribute
onWithOptions key evalEvents options address msg =
    Native.TapBox.onWithOptions key evalEvents options (Signal.message address msg)

{-| Event handler function which takes an evaluation function, 
an Address of Tapbox, a message and returns a list of `Attributes`.
Equivalent of `Html.Events.on`.
-}
on : String
     -> EvalFunction 
     -> Address a
     -> a 
     -> List Attribute
on key evalEvents address msg =
    let 
        options = { stopPropagation = False, preventDefault = False }
    in
        onWithOptions key evalEvents options address msg
        

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
