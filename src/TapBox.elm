module TapBox (TapBox, tapbox, on, click', click, start) where

{-| TapBox wraps "low-level" mouse/touch events on HTML Elements and transforms
them into "high-level" clicks, hiding the user input method of the device 
(mouse, touch or both) and circumventing quirks like the 300ms delay of 'click'
events on touch devices.

Low-level events are mousestart, mouseend, mousemove, mouseout, touchstart,
touchend, touchmove and touchleave.

High-level events are defined as success functions which evaluate past low-level 
events.

# Definition
@docs TapBox, tapbox, on

# Success Functions
@docs click', click, start

-}

import Html exposing (Attribute, text, Html, div)
import Html.Events 
import Json.Decode as Json
import Time exposing (Time, millisecond, timestamp, second)
import Regex exposing (..)
import String
import Signal exposing (Address)

type Device = Mouse 
            | Touch
type Action = Start 
            | End 
            | Leave 
            | Move

type alias Event = 
    ((Device, Action), Time)

{-| Past events are stored in a String and accessed with regular expressions.
-}
type alias PastEvents = List Event

{-| TapModel represents the past of mouse/touch events. Stores them relative to
`timebase`. Associates a success function with a user defined value.
-}
type alias TapModel a =
    { pastEvents : PastEvents
    , success : Maybe ((PastEvents -> Bool), a)
    , timebase : Time
    }

init : TapModel a
init = 
    { pastEvents = []
    , success = Nothing
    , timebase = 0
    }

{-| Update the TapModel with the incoming event.
* Prune events older than `prune`
* Reset timebase and adjust past events after a time of `prune`
-}
update : Time 
    -> (Time, Maybe (((Device, Action), (PastEvents -> Bool)), a)) 
    -> TapModel a 
    -> TapModel a
update prune event model =
    let t = fst event
        e = snd event
    in
    case e of 
        Nothing -> model
        Just ev -> 
            let 
                newTimebase = 
                    if (model.timebase + prune) < (t - prune)
                        then t - prune
                        else model.timebase
                newEventTime = 
                    t - newTimebase
                newEvent = 
                    (fst (fst ev), newEventTime)
                success = 
                    (snd (fst ev), snd ev)
                pruneBelow = 
                    t - model.timebase - prune
                newPastEvents = 
                    adjustPastEvents (newTimebase - model.timebase) 
                    <| pruneEvents pruneBelow model.pastEvents
            in 
               { model | pastEvents <- newEvent :: newPastEvents
                       , success <- Just success
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
            (e,t-diffTimebase)
    in
        if diffTimebase == 0 
            then events
            else 
                List.map adjustTime events

{-| Apply the success function to the past events.
-}
parseAction : TapModel a -> Maybe a
parseAction model =
    case model.success of
        Nothing -> Nothing
        Just s -> 
            if (fst s) model.pastEvents
               then Just (snd s)
               else Nothing

-- SIGNALS

{-| A Mailbox wrapping the user defined action in Event.
-}
touches : a -> Signal.Mailbox (Maybe (((Device,Action), (PastEvents -> Bool)), a))
touches a =
    Signal.mailbox Nothing

minLast : Time
minLast = (700*Time.millisecond)

-- API

{-| TapBox functions like a Signal.Mailbox. The difference is that it exposes
event handlers instead of an address. `signal` yields a Signal of user defined
type given the result of the success function (PastEvents -> Bool).

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

Note that `on` and `onWithOptions` return a List of Attributes (for each event
one).
-}
type alias TapBox a = 
    { signal : Signal a
    , address : Signal.Address (Maybe (((Device, Action), (PastEvents -> Bool)), a))
    }

{-| Construct a TapBox given a default value of user defined type and the time
events reside in the buffer before they are pruned. 

Creates an internal Signal.Mailbox and wires its address with mouse and touch
event handlers. The signal is folded into an internal model which represents the
past of the events within `prune` time. On any event the success function is 
applied to the past events. 
-}
tapbox : a -> Time -> TapBox a
tapbox noop prune = 
    let tMailbox = touches noop

    in 
        { address = tMailbox.address
        , signal = 
            Signal.filterMap parseAction noop 
                <| Signal.foldp (update prune) init
                <| timestamp tMailbox.signal
        }

{-| Event handler function which takes a success function, an Address of Tapbox,
a message and returns a list of `Attributes`.
-}
on : (PastEvents -> Bool) 
    -> Address (Maybe (((Device,Action), (PastEvents -> Bool)), a)) 
    -> a 
    -> List Attribute
on evalEvents address msg =
    let 
        options = { stopPropagation = False, preventDefault = False }
        helper event x = 
            Html.Events.onWithOptions event options Json.value (\_ -> Signal.message 
                (Signal.forwardTo address Just) 
            x)
    in
        [helper "touchstart" (((Touch, Start), evalEvents), msg)
        ,helper "touchmove" (((Touch, Move), evalEvents), msg)
        ,helper "touchleave" (((Touch, Leave), evalEvents), msg)
        ,helper "touchend" (((Touch, End), evalEvents), msg)
        ,helper "mousedown" (((Mouse, Start), evalEvents), msg)
        ,helper "mousemove" (((Mouse, Move), evalEvents), msg)
        ,helper "mouseout" (((Mouse, Leave), evalEvents), msg)
        ,helper "mouseup" (((Mouse, End), evalEvents), msg)
        ]


{-| A click success function which has to be parametrized with the maximum range
between the start and end event (ie. mousedown/mouseup or touchstart/touchend).

Click searches past events for a pattern of mouseend (mousemove{0,5}) mousestart
or touchend (touchmove|mouseevent){0|5} touchstart. The latter ignores virtual 
mouse events intermingling with a fast series of touches.
-}

click' : Time -> PastEvents -> Bool
click' maxRange events =
    let
        diffTouch = 
            case events of
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
               case (diffMouse,lastTouch) of
                   (Just d, Nothing) -> 
                       d < maxRange
                   (Just d, Just l) ->
                       d < maxRange && l > minLast
                   _ -> False
           Just d ->
               d < maxRange

{-| A convenient click success function in the sense of start and end happening 
within 300 ms and not past virtual mouse event must exist within the last 700 ms
-}
click : PastEvents -> Bool
click = click' (300*millisecond)

{-| A simple mousestart/touchstart success function. If you want a maximum of 
    responsiveness.
-}
start : PastEvents -> Bool
start events =
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
