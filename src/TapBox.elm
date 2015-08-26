module TapBox (TapBox, tapbox, click', click, start, diff, getTime, getSubmatch) where

{-| TapBox wraps "low-level" mouse/touch events on HTML Elements and transforms
them into "high-level" clicks, hiding the user input method of the device 
(mouse, touch or both) and circumventing quirks like the 300ms delay of 'click'
events on touch devices.

Low-level events are mousestart, mouseend, mousemove, mouseout, touchstart,
touchend, touchmove and touchleave.

High-level events are defined as success functions which evaluate past low-level 
events.

# Definition
@docs TapBox, tapbox

# Success Functions
@docs click', click, start

# Utility stuff
@docs getSubmatch, getTime, diff

-}

import Html exposing (Attribute, text, Html, div)
import Html.Events 
import Json.Decode as Json
import Time exposing (Time, millisecond, timestamp, second)
import Regex exposing (..)
import String

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
type alias PastEvents = String

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
    { pastEvents = ""
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
                    eventToString (fst (fst ev), newEventTime)
                success = 
                    (snd (fst ev), snd ev)
                pruneBelow = 
                    t - model.timebase - prune
                newPastEvents = 
                    adjustPastEvents (newTimebase - model.timebase) 
                    <| pruneEvents pruneBelow model.pastEvents
            in 
               { model | pastEvents <- newEvent ++ newPastEvents
                       , success <- Just success
                       , timebase <- newTimebase
               }

{-| Prune past events below `pruneBelow`.
-}
pruneEvents : Time -> PastEvents -> PastEvents
pruneEvents pruneBelow events =
    if pruneBelow <= 0 then events
       else replace All (regex "[MT][selm](\\d+)") (pruneEvent pruneBelow) events

{-| Prune an event if its timestamp is below `pruneBelow`.
-}
pruneEvent : Time -> Match -> String
pruneEvent pruneBelow match =
    let event = match.match
    in
        case List.head match.submatches of
            Nothing -> ""
            Just h -> case h of
                Nothing -> ""
                Just t -> case String.toFloat t of
                    Err x -> ""
                    Ok time -> if time < pruneBelow
                                  then ""
                                  else event

{-| Adjust past events to a new timebase.
-}
adjustPastEvents : Time -> PastEvents -> PastEvents
adjustPastEvents diffTimebase events =
    if diffTimebase == 0 then events
       else replace All 
        (regex "\\d+" )
        (adjustTime diffTimebase) 
        events

{-| Subtract `diff` from the times in a match of timestamps.
-}
adjustTime: Time -> Match -> String
adjustTime diff match =
    let time = match.match
    in case String.toFloat time of
        Err x -> match.match
        Ok t -> toString (t-diff)

{-| Give the String representation of an Event for storing in `PastEvents`.
-}
eventToString : Event -> String
eventToString ((device,action),time) =
    (deviceToString device) ++ (actionToString action) ++ (toString time)

{-| Give the String representation of a Device type for storing in `PastEvents`.
-}
deviceToString : Device -> String
deviceToString device =
    toString device |> String.left 1 

{-| Give the String representation of an Action type for storing in `PastEvents`
-}
actionToString : Action -> String
actionToString action =
    toString action |> String.left 1 |> String.toLower 

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

{-| Subtract two Maybe Times. 
-}
diff : Maybe Time -> Maybe Time -> Maybe Time
diff t1 t2 =
    case t1 of
       Nothing -> Nothing
       Just t1 -> case t2 of
           Nothing -> Nothing
           Just t2 -> Just (t1-t2)

{-| Get a certain String from the first of Regex.find matches given its position
in the submatches list.
-}
getSubmatch : List Match -> Int -> Maybe String
getSubmatch matches position =
    let submatch = case List.head matches of
            Nothing -> Nothing
            Just m -> case List.head <| List.drop (position-1) m.submatches of
                Nothing -> Nothing
                Just h -> h
    in submatch

{-| `getSubmatch` and turn it into Time.
-}
getTime : List Match -> Int -> Maybe Time
getTime matches position =
    case getSubmatch matches position of
        Nothing -> Nothing
        Just d -> case String.toFloat d of
            Err x -> Nothing
            Ok di -> Just di

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

    view on model =
      div [] 
        [ button ( on click Decrement ) [ text "-" ]
        , div [] [ text (toString model) ]
        , button ( on click Increment ) [ text "+" ]
        ]

    update action model =
      case action of
        Increment -> model + 1
        Decrement -> model - 1
        
    main = Signal.map (view myTapBox.on) 
           <| Signal.foldp update 0 myTapBox.signal

Note that `on` and `onWithOptions` return a List of Attributes (for each event
one).
-}
type alias TapBox a = 
    { on: ((PastEvents -> Bool) -> a -> List Attribute)
    , onWithOptions : 
        (Html.Events.Options -> (PastEvents -> Bool) -> a -> List Attribute)
    , signal : Signal a
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
                    <| Signal.foldp (update prune) init
                    <| timestamp mailbox.signal
        }

{-| A click success function which has to be parametrized with the maximum range
between the start and end event (ie. mousedown/mouseup or touchstart/touchend).

Click searches past events for a pattern of mouseend (mousemove{0,5}) mousestart
or touchend (touchmove|mouseevent){0|5} touchstart. The latter ignores virtual 
mouse events intermingling with a fast series of touches.
-}

click' : Time -> PastEvents -> Bool
click' maxRange events =
    let matchTouch = 
            find (AtMost 1) (regex
                <| "^"
                ++ "Te(\\d+)" 
                    ++ "(?:" 
                    ++ "(?:Tm\\d+)"
                    ++ "|"
                    ++ "(?:M.\\d+)"
                    ++ "){0,5}"
                ++ "Ts(\\d+)"
            )
            events
        matchMouse = 
            find (AtMost 1) (regex
                <| "^"
                ++ "Me(\\d+)"
                ++ "(?:Mm\\d+){0,5}"
                ++ "Ms(\\d+)"
                ++ "[^T]*(?:T.(\\d+))?"
            )
            events
        matches = 
            if List.isEmpty matchTouch 
               then matchMouse 
               else matchTouch

        time1 = getTime matches 1
        time2 = getTime matches 2
        last = getTime matches 3

    in
       case diff time1 time2 of
           Nothing -> False
           Just d ->
               d < maxRange -- end has to happen within `maxRange` after start 
       && case diff time2 last of
           Nothing -> True
           Just l -> 
               l > minLast -- at least `minLast` time after 

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
    let match =
        find (AtMost 1) 
             (regex <| "^"
                ++ "([MT])s(\\d+)"
                ++ "[^T]*(?:T.(\\d+))?"
             )
             events
        device = getSubmatch match 1
        time = getTime match 2
        last = case device of
            Nothing -> Nothing
            Just d -> if d == deviceToString Mouse
                         then getTime match 3
                         else Nothing
    in
       if time == Nothing then False
       else case diff time last of 
           Nothing -> True
           Just l -> l > minLast
