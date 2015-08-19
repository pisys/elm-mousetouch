module Tap (TapBox,tapbox,click) where
import Html exposing (Attribute, text, Html, div)
import Html.Events 
import Json.Decode as Json
import Debug
import Time

type alias TapBox a = {
        on: ((PastEvents -> Bool) -> a -> List Attribute),
        onWithOptions : (Html.Events.Options -> (PastEvents -> Bool) -> a -> List Attribute),
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
                    <| Signal.foldp updateModel init 
                    <| Time.timestamp mailbox.signal
        }

type Device = Mouse | Touch
type Action = Start | End | Leave | Move

touches : a -> Signal.Mailbox (Maybe (((Device,Action), (PastEvents -> Bool)), a))
touches a =
    Signal.mailbox Nothing

init : TapModel a
init = {
    pastEvents = [],
    success = Nothing
    }

type alias Event = ((Device, Action), Time.Time)

type alias PastEvents = List Event

type alias TapModel a =
    { pastEvents : PastEvents
    , success : Maybe ((PastEvents -> Bool), a)
    }

updateModel : (Time.Time, Maybe (((Device, Action), (PastEvents -> Bool)), a)) -> TapModel a -> TapModel a
updateModel tap model =
    let t = fst tap
        e = snd tap
    in
    case e of 
        Nothing -> model
        Just ev -> 
            let newEvent = (fst (fst ev), t)
                success = (snd (fst ev), snd ev)

                -- throw away events which are older than one second
                newPastEvents = throwAwaySince model.pastEvents t Time.second
            in 
               { model | pastEvents <- (Debug.log "newEvent" newEvent ) :: (Debug.log "pastEvents" newPastEvents)
                       , success <- Just success
               }

throwAwaySince : PastEvents -> Time.Time -> Time.Time -> PastEvents
throwAwaySince list now since =
    fst (List.partition (\(_,t') -> t' > (now - since)) list)

parseInput : TapModel a -> TapModel a
parseInput model = 
    { model | pastEvents <- (Debug.log "after twoStarts" (twoStarts model.pastEvents))}

-- reduce two consecutive mouse/touch start events to one
twoStarts : PastEvents -> PastEvents
twoStarts list =
    let head = List.take 2 list
        tail = List.drop 2 list
    in
       if List.all (\((_,a),t) -> a == Start) head
          then (List.take 1 head) `List.append` tail
          else list

parseAction : TapModel a -> Maybe a
parseAction model =
    case model.success of
        Nothing -> Nothing
        Just s -> 
            if (Debug.log "parseAction, success" ((fst s) model.pastEvents))
               then Just (snd s)
               else Nothing

click : PastEvents -> Bool
click list =
    let head = Debug.log "click head" (List.take 2 list)
        first = Debug.log "click first" (List.head head)
        second = Debug.log "click second" (List.tail head)
    in
       case second of
           Nothing -> False
           Just l -> 
               case List.head l of
                   Nothing -> False
                   Just s ->
                       case first of 
                           Nothing -> False
                           Just f ->
                               fst(fst(f)) == fst(fst(s)) -- comparing Devices
                               && snd(fst(f)) == End -- comparing Actions
                               && snd(fst(s)) == Start
                               && (snd f) < ((snd s) + (300*Time.millisecond))


