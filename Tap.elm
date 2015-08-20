module Tap (TapBox,tapbox,click) where
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

                newEventTime = t - (Debug.log "newTimebase" newTimebase)

                newEvent = 
                    Debug.log "newEvent" (
                        eventToString (fst (fst ev), newEventTime)
                    )
                success = (snd (fst ev), snd ev)

                pruneBelow = Debug.log "pruneBelow" (t - model.timebase - prune)

                -- throw away events which are older than one second
                newPastEvents = adjustPastEvents (newTimebase - model.timebase) 
                    <| pruneEvents pruneBelow model.pastEvents
            in 
               { model | pastEvents <- newEvent ++ (Debug.log "pastEvents" newPastEvents)
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

eventToString : Event -> String
eventToString ((device,action),time) =
    (deviceToString device) ++ (actionToString action) ++ (toString time)

deviceToString : Device -> String
deviceToString device =
    toString device |> String.left 1 

actionToString : Action -> String
actionToString action =
    toString action |> String.left 1 |> String.toLower 

throwAwaySince : Time -> PastEvents -> PastEvents
throwAwaySince since events =
    let es = find All (regex (Debug.log "throwAway, regexAnyEvent" regexAnyEvent)) events
    in .string <| List.foldl (keepBefore since) {string = "", cum = 0} es
    --fst (List.partition (\(_,t') -> t' > (now - since)) list)

keepBefore : Time -> Match -> {string: String, cum : Float} -> {string:String, cum: Float}
keepBefore since match past =
    {-- string = if past.cum > since then "true" else "false"
    --, cum = if past.cum > since then 0 else 1
    --}
    let 
        matches = Debug.log "keepBefore, match" match
        diff = case List.head match.submatches of
            Nothing -> 0
            Just s -> case s of
                Nothing -> 0
                Just d -> case String.toFloat d of
                    Ok dd -> dd
                    Err x -> 0
        cum = past.cum + diff
    in
    Debug.log "keepBefore" { string = if cum < since then past.string ++ match.match else past.string 
    , cum = cum
    }

adjustPastEvents : Time -> PastEvents -> PastEvents
adjustPastEvents diffTimebase events =
    if diffTimebase == 0 then events
       else replace All 
        (regex (Debug.log "adjustPastEvents, regexAnyEvent" regexDiff) )
        (adjustTime diffTimebase) 
        (Debug.log "adjustPastEvents, events" events)

adjustTime: Time -> Match -> String
adjustTime diff match =
    let time = match.match
    in case String.toFloat time of
        Err x -> match.match
        Ok t -> toString (t-diff)


subDiffOrRemove : Time -> Time -> String -> List (Maybe String) -> String
subDiffOrRemove diffTimebase pruneLevel event matches =
    case List.head matches of
        Nothing -> ""
        Just s -> case s of
            Nothing -> ""
            Just d -> case String.toFloat (Debug.log "subdiff, time =" d) of
                Err x -> ""
                Ok t -> if t < diffTimebase
                    then ""
                    else replace (AtMost 1) (regex regexDiff) 
                        (\{match} -> subDiff match diffTimebase) event

subDiff : String -> Time -> String
subDiff t diff =
    case String.toFloat t of
        Err x -> "0"
        Ok ttl -> toString (ttl-diff)

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
    ) (\m -> (Debug.log "match" m).match ) events

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
                    ++ buildRegex [Touch] [Start] True
                )
                events
        matchMouse = Debug.log "mouseclick head" 
            <| find (AtMost 1) (regex
                    <| "^"
                    ++ buildRegex [Mouse] [End] True
                    ++ buildRegex [Mouse] [Start] True
                )
                events
        diff = 
            if List.isEmpty matchTouch 
               then getDiff matchMouse 
               else getDiff matchTouch

        last =
            if List.isEmpty matchTouch
               then getLast (deviceToString Touch) events
               else Nothing --getLast "Mouse" events

        getDiff : List Match -> Maybe Time
        getDiff matches =
            case List.head matches of
                Nothing -> Nothing
                Just match -> 
                    case List.head (List.reverse match.submatches) of
                        Nothing -> Nothing
                        Just h -> 
                            case h of 
                                Nothing -> Nothing
                                Just d ->
                                    case String.toFloat d of
                                        Err x -> Nothing
                                        Ok di -> Just di

    in
       case diff of
           Nothing -> False
           Just diff ->
               diff < range
               -- means: touchend needs to happen within 300ms after start 
       && case Debug.log "last" last of
           Nothing -> True
           Just l -> 
               l > recent
               -- means: at least 300ms after a mouse event (or vice-versa)
        
getLast : String -> PastEvents -> Maybe Time
getLast device events =
    let es = find All (regex <| buildRegexFromString 
                                    (remember regexAnyDevice) 
                                    (dontremember regexAnyAction)
                                    (remember regexDiff)
                      ) events
            |> List.drop 1 -- drop the first event since it contains the absolute timestamp
    in case List.foldl (accumDiffs device) (Just {cum=0, stop=False}) (Debug.log "getLast events" es) of
        Nothing -> Nothing
        Just {cum,stop} -> 
            if stop 
               then Just cum
               else Nothing


accumDiffs : String -> Match -> Maybe {cum : Time, stop: Bool} -> Maybe {cum : Time, stop: Bool}
accumDiffs stopat match cum =
    case cum of
        Nothing -> Nothing
        Just c -> case List.head match.submatches of
            Nothing -> Nothing
            Just h -> case h of
                Nothing -> Nothing
                Just device -> case List.head (List.reverse match.submatches) of
                    Nothing -> Nothing
                    Just h2 -> case h2 of
                        Nothing -> Nothing
                        Just d -> case String.toFloat d of
                            Err x -> Nothing
                            Ok di -> 
                                Just { c | cum <- if not c.stop 
                                                       then c.cum + di
                                                       else c.cum 
                                         , stop <- c.stop || device == stopat }

