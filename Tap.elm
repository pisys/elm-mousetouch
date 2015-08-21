module Tap (TapBox,tapbox,click, normalclick, start) where
import Html exposing (Attribute, text, Html, div)
import Html.Events 
import Json.Decode as Json
import Debug
import Time exposing (Time, millisecond, timestamp, second)
import Regex exposing (..)
import String

type alias TapBox a = {
        on: ((PastEvents -> Bool) -> a -> List Attribute),
        onWithOptions : (Html.Events.Options -> (PastEvents -> Bool) -> a -> List Attribute),
        signal : Signal a
    }

tapbox : a -> Time -> TapBox a
tapbox noop prune = 
    let mailbox = touches noop

        onWithOptions options hle msg =
            let 
                -- always preventDefault to avoid virtual mouseevents after touch events
                op = { stopPropagation = options.stopPropagation
                     , preventDefault = True } 
                helper event x = 
                    Html.Events.onWithOptions event op Json.value 
                        (\_ -> Signal.message 
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
            on = onWithOptions {stopPropagation = False, preventDefault = True},
            signal = 
                Signal.filterMap parseAction noop 
                    <| Signal.map parseInput
                    <| Signal.foldp (updateModel prune) init
                    <| timestamp mailbox.signal
        }

type Device = Mouse | Touch
type Action = Start | End | Leave | Move

touches : a -> Signal.Mailbox (Maybe (((Device,Action), (PastEvents -> Bool)), a))
touches a =
    Signal.mailbox Nothing

init : TapModel a
init = {
    pastEvents = "",
    success = Nothing,
    timebase = 0
    }

type alias Event = ((Device, Action), Time)

type alias PastEvents = String

type alias TapModel a =
    { pastEvents : PastEvents
    , success : Maybe ((PastEvents -> Bool), a)
    , timebase : Time
    }

updateModel : Time 
    -> (Time, Maybe (((Device, Action), (PastEvents -> Bool)), a)) 
    -> TapModel a -> TapModel a
updateModel prune tap model =
    let t = fst tap
        e = snd tap
    in
    case e of 
        Nothing -> model
        Just ev -> 
            let 
                newTimebase = if (model.timebase + prune) < (t - prune)
                                 then t - prune
                                 else model.timebase

                newEventTime = t - newTimebase

                newEvent = 
                    Debug.log "newEvent" (
                        eventToString (fst (fst ev), newEventTime)
                    )
                success = (snd (fst ev), Debug.log "action" (snd ev))

                pruneBelow = t - model.timebase - prune

                newPastEvents = adjustPastEvents (newTimebase - model.timebase) 
                    <| pruneEvents pruneBelow model.pastEvents
            in 
               { model | pastEvents <- newEvent ++ newPastEvents
                       , success <- Just success
                       , timebase <- newTimebase
               }

pruneEvents : Time -> PastEvents -> PastEvents
pruneEvents pruneBelow events =
    if pruneBelow <= 0 then events
       else replace All (regex regexAnyEvent) (pruneEvent pruneBelow) events

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

adjustPastEvents : Time -> PastEvents -> PastEvents
adjustPastEvents diffTimebase events =
    if diffTimebase == 0 then events
       else replace All 
        (regex regexDiff )
        (adjustTime diffTimebase) 
        events

adjustTime: Time -> Match -> String
adjustTime diff match =
    let time = match.match
    in case String.toFloat time of
        Err x -> match.match
        Ok t -> toString (t-diff)

eventToString : Event -> String
eventToString ((device,action),time) =
    (deviceToString device) ++ (actionToString action) ++ (toString time)

deviceToString : Device -> String
deviceToString device =
    toString device |> String.left 1 

actionToString : Action -> String
actionToString action =
    toString action |> String.left 1 |> String.toLower 


buildRegexFromString : String -> String -> String -> String
buildRegexFromString device action ttl =
    device ++ action ++ ttl
        
buildRegex : List Device -> List Action -> Bool -> String
buildRegex device action ttlRemember =
    let rem = if ttlRemember then remember else dontremember
    in (buildRegexDevice device)
        ++ (buildRegexAction action)
        ++ (rem regexDiff)

buildRegexDevice : List Device -> String
buildRegexDevice device =
    let b = if List.length device > 1 then ("[","]") else ("","")
    in (fst b) 
        ++ (List.foldl (\d -> \dd -> dd ++ deviceToString d) "" device) 
        ++ (snd b) 

buildRegexAction : List Action -> String
buildRegexAction action =
    let b = if List.length action > 1 then ("[","]") else ("","")
    in (fst b)
        ++ (List.foldl (\a -> \aa -> aa ++ actionToString a) "" action) 
        ++ (snd b) 


regexAnyEvent : String
regexAnyEvent = buildRegexFromString 
    regexAnyDevice
    regexAnyAction 
    (remember regexDiff)

regexDiff : String
regexDiff = "\\d+"

dontremember : String -> String
dontremember string = "(?:"++string++")"

remember : String -> String
remember string = "("++string++")"

regexAnyMouseEvent : String
regexAnyMouseEvent = buildRegex [Mouse] [Start,End,Leave,Move] True

regexAnyTouchEvent : String
regexAnyTouchEvent = buildRegex [Touch] [Start,End,Leave,Move] True

regexAnyAction : String
regexAnyAction = buildRegexAction [Start,End,Leave,Move] 

regexAnyDevice : String
regexAnyDevice = buildRegexDevice [Mouse,Touch]

parseInput : TapModel a -> TapModel a
parseInput model = 
    model
    --{ model | pastEvents <- (Debug.log "after twoStarts" (twoStarts model.pastEvents))}

-- reduce two consecutive mouse/touch start events to one
twoStarts : PastEvents -> PastEvents
twoStarts events =
    replace (AtMost 1) (
        "(" ++ (buildRegex [Mouse,Touch] [Start] True) ++ "){2}" |> regex
    ) (\m -> m.match ) events

parseAction : TapModel a -> Maybe a
parseAction model =
    case model.success of
        Nothing -> Nothing
        Just s -> 
            if (Debug.log "parseAction, success" ((fst s) model.pastEvents))
               then Just (snd s)
               else Nothing

click : Time -> Time -> PastEvents -> Bool
click range recent events =
    let matchTouch = Debug.log "touchclick head" 
            <| find (AtMost 1) (regex
                    <| "^"
                    ++ buildRegex [Touch] [End] True
                    ++ (dontremember 
                            (buildRegex [Touch] [Move] False) 
                       ) ++ "{0,5}"
                    ++ buildRegex [Touch] [Start] True
                )
                events
        matchMouse = Debug.log "mouseclick head" 
            <| find (AtMost 1) (regex
                    <| "^"
                    ++ buildRegex [Mouse] [End] True
                    ++ dontremember (buildRegex [Mouse] [Move] False)
                    ++ "{0,5}"
                    ++ buildRegex [Mouse] [Start] True
                )
                events
        matches = 
            if List.isEmpty matchTouch 
               then matchMouse 
               else matchTouch

        time1 = getTime matches 1
        time2 = getTime matches 2

        last =
            if List.isEmpty matchTouch
               then getLast Touch events
               else Nothing --getLast "Mouse" events

    in
       case Debug.log "diff" (diff time1 time2) of
           Nothing -> False
           Just d ->
               d < range
               -- means: touchend needs to happen within 300ms after start 
       && case Debug.log "last" (diff time2 last) of
           Nothing -> True
           Just l -> 
               l > recent
               -- means: at least 300ms after a mouse event (or vice-versa)

diff : Maybe Time -> Maybe Time -> Maybe Time
diff t1 t2 =
   case t1 of
       Nothing -> Nothing
       Just t1 -> case t2 of
           Nothing -> Nothing
           Just t2 -> Just (t1-t2)

start : PastEvents -> Bool
start events =
    not <| List.isEmpty 
        <| find (AtMost 1) 
             (regex <| "^" ++ buildRegex [Mouse,Touch] [Start] False)
             events
        
getTime : List Match -> Int -> Maybe Time
getTime matches position =
    let submatch = case List.head matches of
            Nothing -> Nothing
            Just m -> case List.head <| List.drop (position-1) m.submatches of
                Nothing -> Nothing
                Just h -> h
    in case submatch of 
        Nothing -> Nothing
        Just d ->
            case String.toFloat d of
                Err x -> Nothing
                Ok di -> Just di

getLast : Device -> PastEvents -> Maybe Time
getLast device events =
    let es = find (AtMost 1) (
                    regex <| buildRegex [device] [Start,End,Move,Leave] True
                ) events
    in getTime es 1


normalclick = click (300*millisecond) (700*millisecond)

